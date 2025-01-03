import * as CST from '../visitor/CST.js';
import * as Template from './PlantillaFortran.js';
import { getActionId, getReturnType, getExprId, getRuleId } from './utils.js';

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/**
 * @implements {Visitor}
 */
export default class FortranTranslator {
    /** @type {ActionTypes} */
    actionReturnTypes;
    /** @type {string[]} */
    actions;
    /** @type {boolean} */
    translatingStart;
    /** @type {string} */
    currentRule;
    /** @type {number} */
    currentChoice;
    /** @type {number} */
    currentExpr;

    /**
     *
     * @param {ActionTypes} returnTypes
     */
    constructor(returnTypes) {
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
    }

    /**
     * @param {CST.Grammar} node
     * @this {Visitor}
     */
    visitGrammar(node) {
        const rules = node.rules.map((rule) => rule.accept(this));

        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            rules,
        });
    }

    /**
     * @param {CST.Regla} node
     * @this {Visitor}
     */
    visitRegla(node) {
        this.currentRule = node.id;
        this.currentChoice = 0;

        if (node.start) this.translatingStart = true;

        const ruleTranslation = Template.rule({
            id: node.id,
            returnType: getReturnType(
                getActionId(node.id, this.currentChoice),
                this.actionReturnTypes
            ),
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>
                election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .map((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;
                        return `${
                            expr instanceof CST.Identificador
                                ? getReturnType(
                                      getActionId(expr.id, i),
                                      this.actionReturnTypes
                                  )
                                : 'character(len=:), allocatable'
                        } :: expr_${i}_${j}`;
                    })
            ),
            expr: node.expr.accept(this),
        });

        this.translatingStart = false;

        return ruleTranslation;
    }

    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
        });
    }

    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `expr_${this.currentChoice}_${i}`
        );

        /** @type {string[]} */
        let neededExprs;
        /** @type {string} */
        let resultExpr;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],
            });
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));
        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr,
        });
    }

    /**
     * @param {CST.Pluck} node
     * @this {Visitor}
     */
    visitPluck(node) {
        return node.labeledExpr.accept(this);
    }

    /**
     * @param {CST.Label} node
     * @this {Visitor}
     */
    visitLabel(node) {
        return node.annotatedExpr.accept(this);
    }

    /**
     * @param {CST.Annotated} node
     * @this {Visitor}
     */
    visitAnnotated(node) {
        if (node.qty && typeof node.qty === 'string') {
            if (node.expr instanceof CST.Identificador) {
                // TODO: Implement quantifiers (i.e., ?, *, +)
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({
                quantifier: node.qty,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty !== '+' && node.qty !== '*' && node.qty !== '?' && node.qty !== null) {
                if(node.qty instanceof Object){


                    switch (node.qty.type) {

                        case "conteo":
                            if(node.qty.value instanceof CST.Predicate){
                                return `print *, "funcion no implementada"`
                            }else{

                            
                            return `
                            rep = 0
                            do j= 0, ${node.qty.value}
                                if (${node.expr.accept(this)}) then
                                    rep = rep + 1 
                                else
                                    exit  ! Salir si no hay coincidencia
                                end if
                            end do
                            if (rep == ${node.qty.value}) then
                                res = consumeInput()  ! Es válido si el conteo es igual a qty_int
                                rep = 0
                                return
                            end if
                            cycle
                            `;
                        }
                            break;
                        case "conteo1":
                            let minino =  node.qty.value[0]
                            let maximo =  node.qty.value[1] 
                            
                            if(minino instanceof CST.Predicate && maximo instanceof CST.Predicate){
                                return `print *, "funcion no implementada"`

                            }else{

                            
                            return `
                            rep = 0
                                do j = 0, ${maximo}
                                if (${node.expr.accept(this)}) then
                                    rep = rep + 1
                                else
                                    exit

                                end if

                            end do

                            ! Validar que estemos dentro del rango [minimo, maximo]
                            if (rep >= ${minino} .and. rep <= ${maximo}) then
                                res = consumeInput() 
                                rep = 0
                                return
                            end if
                            cycle`
                        }
                            break;

                        case "conteo2":
                            const minVal = node.qty.value[0]; // Valor mínimo
                            const delimiterval = node.qty.value[1].replace(/['"]/g, ''); // Delimitador


                            if(minVal instanceof CST.Predicate){
                                return `print *, "funcion no implementada"`
                            }else{
                            
        
                            return `
                            rep = 0
                                
                                if (${node.expr.accept(this)}) rep = rep + 1
                                do j = 0, ${minVal}
                                    if (.not. acceptString('${delimiterval}', .false.)) exit
                                    if (${node.expr.accept(this)}) then
                                        rep = rep + 1
                                        else
                                            exit  ! Salir si no hay coincidencia
                                        end if
                                    end do
                                ! Verificar si necesitamos un delimitador
                                if (rep == ${minVal}) then
                                    res = consumeInput()  ! Es válido si el conteo es igual a qty_int
                                    rep = 0
                                    return
                                end if
                                cycle
                            `;
                            }
                            break;
                        
                        case "conteo3":

                            const min = node.qty.value[0]; // Valor mínimo
                            const max = node.qty.value[1]; // Valor máximo
                            const delimiter = node.qty.value[2].replace(/['"]/g, ''); // Delimitador

                            if(min instanceof CST.Predicate && max instanceof CST.Predicate){
                                return `print *, "funcion no implementada"`

                            }else{
                            
        
                            return `
                            rep = 0
    
                            
                                
                                if (${node.expr.accept(this)}) rep = rep + 1
                                do j = 0, ${max}
                                    if (.not. acceptString('${delimiter}', .false.)) exit
                                    if (${node.expr.accept(this)}) then
                                        rep = rep + 1
                                        else
                                            exit  ! Salir si no hay coincidencia
                                        end if
                                    end do
                                ! Verificar si necesitamos un delimitador
                                if (rep >= ${min} .and. rep <= ${max}) then
                                    res = consumeInput()  ! Es válido si el conteo es igual a qty_int
                                    rep = 0
                                    return
                                end if
                                cycle
                            `;
                            }
                        
                        default:
                            break;


                    }


                    

                }
            
        } else {
            if (node.expr instanceof CST.Identificador) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        }
    }

    /**
     * @param {CST.Assertion} node
     * @this {Visitor}
     */
    visitAssertion(node) {
        if(node.assertion instanceof CST.Predicate){
            return `print *, "funcion no implementada"` ;
        }else {
            const translation = node.assertion.accept(this);
            return Template.assertionExpression({
                expr: translation
            });
        }
    }

    /**
     * @param {CST.NegAssertion} node
     * @this {Visitor}
     */
    visitNegAssertion(node) {
        if (node.assertion instanceof CST.Predicate ){
            return `print *, "funcion no implementada"`
            ;            
        } else{
            const translation = node.assertion.accept(this);
            return Template.assertionNegExpression({
                expr: translation
            });
        }
    }

    /**
     * @param {CST.Predicate} node
     * @this {Visitor}
     */
    visitPredicate(node) {
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, ruleId]) =>
                    `${getReturnType(
                        getActionId(ruleId, this.currentChoice),
                        this.actionReturnTypes
                    )} :: ${label}`
            ),
            code: node.code,
        });
    }

    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        return `acceptString('${node.val}', ${node.isCase ? '.true.' : '.false.'})`;
    }

    /**
     * @param {CST.Clase} node
     * @this {Visitor}
     */
    visitClase(node) {
        // [abc0-9A-Z]
        let characterClass = [];
        const set = node.chars
        .filter((char) => char instanceof CST.literalRango)
            .map((char) => `'${char.contenido}'`);

        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => {
                // Verificar si `isCase` está activo para este nodo
                if (node.isCase) {
                    return `acceptRangeCaseInsensitive('${range.bottom}', '${range.top}')`;
                } else {
                    return `acceptRange('${range.bottom}', '${range.top}')`;
                }
            });

         //recorre el arreglo set buscando caracteres especiales y agregandolos al arreglo especiales
         let especiales  = [];
         for (let i = 0; i < set.length; i++) {
             console.log(set[i],"0");               
             
             if ( String(set[i]) == "'\\n'") {   
                 console.log("salto");                                     
                 especiales.push(`acceptString(char(10),.false.)`); // Agregamos al arreglo especiales
                 set.splice(i, 1); // Eliminamos del arreglo set
                 i--; // Decrementamos i para no saltar el siguiente elemento
         
             }else if (set[i] ==`'\\t'`) {  
                 console.log("tabular");                                      
                 especiales.push(`acceptString('    ',.false.)`);// Agregamos al arreglo especiales
                 set.splice(i, 1); // Eliminamos del arreglo set
                 i--; // Decrementamos i para no saltar el siguiente elemento
         
             }else if (set[i] ==`'\\r'`) {  
                console.log("tabular");                                      
                especiales.push(`acceptString(achar(13),.false.)`);// Agregamos al arreglo especiales
                set.splice(i, 1); // Eliminamos del arreglo set
                i--; // Decrementamos i para no saltar el siguiente elemento
        
            }            
         }
            
            if (set.length !== 0) {
                if (node.isCase) {
                    characterClass = [`acceptSetCaseInsensitive([${set.join(',')}])`];
                } else {
                    characterClass = [`acceptSet([${set.join(',')}])`];
                }
            }

            // Combinar literales,rangos y especiales
        if (especiales.length !== 0) {
            console.log("juntar especiales");
            
            // Copiar elementos de 'especiales' a 'caracteres'
            characterClass = characterClass.concat(especiales);
        }
        
            // Combinar literales y rangos
            if (ranges.length !== 0) {
                characterClass = [...characterClass, ...ranges];
            }
        return `(${characterClass.join(' .or. ')})`; // acceptSet(['a','b','c']) .or. acceptRange('0','9') .or. acceptRange('A','Z')
    }

    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        return `acceptRange('${node.bottom}', '${node.top}')`;
    }

    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        return getRuleId(node.id) + '()';
    }

    /**
     * @param {CST.Punto} node
     * @this {Visitor}
     */
    visitPunto(node) {
        return 'acceptPeriod()';
    }

    /**
     * @param {CST.Fin} node
     * @this {Visitor}
     */
    visitFin(node) {
        return 'if (.not. acceptEOF()) cycle';
    }

    
     /**
     * @param {CST.literalRango} node
     * @this {Visitor}
     */
     visitliteralRango(node) {
        return node.contenido.accept(this);
    }



         /**
     * @param {CST.visitAgrupacion} node
     * @this {Visitor}
     */
    visitAgrupacion(node){

    }
}