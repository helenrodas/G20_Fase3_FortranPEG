import * as CST from '../visitor/CST.js';

/**
 * @typedef {import('../visitor/Visitor.js').default<string>} Visitor
 */
/**
 * @implements {Visitor}
 */
export default class FortranTranslator {
    /**
     * @param {CST.Producciones} node
     * @this {Visitor}
     */
    visitProducciones(node) {
        return `
        function peg_${node.id}() result(accept)
            logical :: accept
            integer :: i,conteo,j
            integer :: initialCursor

            accept = .false.
            initialCursor = cursor
            ${node.expr.accept(this)}
            ${
                node.start
                    ? `
                    if (.not. acceptEOF()) then
                        return
                    end if
                    `
                    : ''
            }
            accept = .true.
        end function peg_${node.id}
        `;
    }
    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        const template = `
        do i = 0, ${node.exprs.length}
            cursor = initialCursor
            select case(i)
                ${node.exprs
                    .map(
                        (expr, i) => `
                        case(${i})
                            ${expr.accept(this)}
                            exit
                        `
                    )
                    .join('\n')}
            case default
                return
            end select
        end do
        `;
        return template;
    }
    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }
    /**
     * @param {CST.Expresion} node
     * @this {Visitor}
     */
    visitExpresion(node) {
        const condition = node.expr.accept(this);
        if (node.label == '!'){
            return `
                if (${condition}) then
                    print *, "Error: asercion negativa '${node.expr.val}' encontrada"
                    call exit(1)
                end if
        `}

        // if(node.qty instanceof CST.Conteo){
        //     // Si es un string, usamos su valor
        //     const valor = node.expr.val ;

        //     // return `
        //     //     if (.not. (${condition})) then
        //     //         cycle
        //     //     end if
        //     //     if (.not. countRepetitions('${valor}', ${node.qty.conteo1}))then
        //     //         cycle
        //     //     end if
        //     // `;
        // }
        
        if (node.qty !== '+' && node.qty !== '*' && node.qty !== '?' && node.qty !== null) {

            if(node.qty instanceof Array){

                let minino =  node.qty[0]
                let maximo =  node.qty[1]   

                console.log(minino,maximo);
                return `
                conteo = 0
                    do j = 0, ${maximo}
                    if (${condition}) then
                        conteo = conteo + 1
                    else
                            exit
                        end if
                end do
                ! Validar que estemos dentro del rango [minimo, maximo]
                if (conteo >= ${minino} .and. conteo <= ${maximo}) then
                    accept = .true.
                    conteo = 0
                    return
                end if
                cycle`
                
            }
            
            return `
                    conteo = 0
                    do j= 0, ${node.qty}
                        if (${condition}) then
                            conteo = conteo + 1 
                        else
                            exit  ! Salir si no hay coincidencia
                        end if
                    end do
                    if (conteo == ${node.qty}) then
                        accept = .true.  ! Es válido si el conteo es igual a qty_int
                        conteo = 0
                        return
                    end if
                    cycle
                    `;
        }
        switch (node.qty) {
            case '+':
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '*':
                return `
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '?':
                return `
                if (.not. (${condition})) then
                    contiue
                end if
                `;
            default:
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                `;
        }
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
        // [abc0-9A-Z] con opción de case-insensitive (isCase)
        let characterClass = [];
        
        // Procesar caracteres individuales (literalRango)
        const set = node.chars
            .filter((char) => char instanceof CST.literalRango)
            .map((char) => `'${char.contenido}'`); 
    
        // Procesar rangos (Rango)
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
            
                }            
            }
        
    
        // Generar llamada a acceptSet si hay caracteres individuales
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
    
        // Unir todas las condiciones con `.or.` para formar la expresión final
        return characterClass.join(' .or. '); 
    }
    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        return `acceptRange('${node.bottom}', '${node.top}')`;
    }

    /**
     * @param {CST.literalRango} node
     * @this {Visitor}
     */
    visitliteralRango(node) {
        return node.contenido.accept(this);
    }





    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        return `peg_${node.id}()`;
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
        return 'acceptEOF()';
    }

        /**
     * @param {CST.Conteo} node
     * @this {Visitor}
     */
        visitConteo(node) {
            // Accedemos a la expresión guardada
            const valor = this.currentExpression instanceof CST.String ? this.currentExpression.val : 'input(cursor:cursor)';
            return `if (.not. countRepetitions('${valor}', ${node.conteo1}))then
                        cycle
                    end if
            `;
        }

}