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
            integer :: i
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
                    cycle
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
        // [abc0-9A-Z]
        let characterClass = [];
        const set = node.chars
            .filter((char) => char instanceof CST.literalRango)
            .map((char) => `'${char.contenido}'`);
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return characterClass.join(' .or. '); // acceptSet(['a','b','c']) .or. acceptRange('0','9') .or. acceptRange('A','Z')
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
    visitConteo(node){
        return `acceptConteo(${node.conteo1.accept(this)}, '${node.num}')`;
    }


}