//PLantilla que genera el codigo de FOrtran
/**
 *
 * @param {{
*  beforeContains: string
*  afterContains: string
*  startingRuleId: string;
*  startingRuleType: string;
*  rules: string[];
*  actions: string[];
* }} data
* @returns {string}
*/
export const main = (data) => `
!auto-generated
module parser
   implicit none
   character(len=:), allocatable, private :: input,expected
   integer, private :: savePoint, lexemeStart, cursor

   interface toStr
       module procedure intToStr
       module procedure strToStr
   end interface
   
   ${data.beforeContains}

   contains
   
   ${data.afterContains}

   function parse(str) result(res)
       character(len=:), allocatable :: str
       ${data.startingRuleType} :: res

       input = str
       cursor = 1

       res = ${data.startingRuleId}()
   end function parse

   ${data.rules.join('\n')}

   ${data.actions.join('\n')}

    function acceptString(str, isCase) result(accept)
    character(len=*) :: str
    logical :: accept
    logical :: isCase
    integer :: offset
    character(len=len(str)) :: temp_str,strLower,tempLower
    integer :: i, char_code
    offset = len(str) - 1
    temp_str = input(cursor:cursor + offset)
    if (isCase) then

        strLower = toLower(str)
        tempLower =   toLower(input(cursor:cursor + offset))
        if(strLower /= tempLower) then
            accept = .false.
            expected = str
            return
    end if
            do i = 1, len(str)
            char_code = iachar(temp_str(i:i))
            if (char_code >= iachar('a') .and. char_code <= iachar('z')) then
                temp_str(i:i) = achar(char_code - 32)  ! Convertir a mayúscula
            end if
        end do
        
        if (str == temp_str) then
            accept = .true.
            !cursor = cursor + len(str)
            return
        end if
    else
        ! Comparación sensible a mayúsculas/minúsculas
        if (str /= temp_str) then
            accept = .false.
            expected = str
            return
        end if
    end if
    ! Si pasaron las condiciones
    cursor = cursor + len(str)
    accept = .true.
end function acceptString
    function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function tolower

   function acceptRange(bottom, top) result(accept)
       character(len=1) :: bottom, top
       logical :: accept

       if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptRange

       function acceptRangeCaseInsensitive(bottom, top) result(accept)
        character(len=1) :: bottom, top
        logical :: accept
        character(len=1) :: temp_char
        integer :: char_code
        accept = .false.
        ! Verificar que no se exceda el rango de la entrada
        if (cursor > len_trim(input)) then
            return
        end if
        temp_char = input(cursor:cursor)  ! Extraer el carácter actual
        ! Convertir a minúscula si es mayúscula
        char_code = iachar(temp_char)
        if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
            temp_char = achar(char_code + 32)  ! Convertir a minúscula
        end if
        ! Comparar contra el rango
        if (iachar(temp_char) >= iachar(bottom) .and. iachar(temp_char) <= iachar(top)) then
            accept = .true.
            cursor = cursor + 1  ! Avanzar el cursor
        end if
    end function acceptRangeCaseInsensitive

   function acceptSet(set) result(accept)
       character(len=1), dimension(:) :: set
       logical :: accept

       if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptSet

       function acceptSetCaseInsensitive(set) result(accept)
        character(len=1), dimension(:) :: set
        logical :: accept
        character(len=1) :: temp_char
        integer :: char_code, i
        accept = .false.
        ! Verificar que no se exceda el rango de la entrada
        if (cursor > len_trim(input)) then
            return
        end if
        temp_char = input(cursor:cursor)  ! Extraer el carácter actual
        ! Convertir a minúscula si es mayúscula
        char_code = iachar(temp_char)
        if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
            temp_char = achar(char_code + 32)  ! Convertir a minúscula
        end if
        ! Verificar si el carácter está en el conjunto
        do i = 1, size(set)
            if (temp_char == set(i)) then
                accept = .true.
                cursor = cursor + 1  ! Avanzar el cursor
                temp_char = input(cursor:cursor)
                !return
            end if
            cursor = cursor + 1
            accept = .true.
        end do
    end function acceptSetCaseInsensitive

   function acceptPeriod() result(accept)
       logical :: accept

       if (cursor > len(input)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptPeriod

   function acceptEOF() result(accept)
       logical :: accept

       if(.not. cursor > len(input)) then
           accept = .false.
           return
       end if
       accept = .true.
   end function acceptEOF

   function consumeInput() result(substr)
       character(len=:), allocatable :: substr

       substr = input(lexemeStart:cursor - 1)
   end function consumeInput

   subroutine pegError()
       print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

       call exit(1)
   end subroutine pegError

   function intToStr(int) result(cast)
       integer :: int
       character(len=31) :: tmp
       character(len=:), allocatable :: cast

       write(tmp, '(I0)') int
       cast = trim(adjustl(tmp))
   end function intToStr

   function strToStr(str) result(cast)
       character(len=:), allocatable :: str
       character(len=:), allocatable :: cast

       cast = str
   end function strToStr
end module parser
`;

/**
*
* @param {{
*  id: string;
*  returnType: string;
*  exprDeclarations: string[];
*  expr: string;
* }} data
* @returns
*/
export const rule = (data) => `
   function peg_${data.id}() result (res)
       ${data.returnType} :: res
       ${data.exprDeclarations.join('\n')}
       integer :: i

       savePoint = cursor
       ${data.expr}
   end function peg_${data.id}
`;

/**
*
* @param {{
*  exprs: string[]
* }} data
* @returns
*/
export const election = (data) => `
       do i = 0, ${data.exprs.length}
           select case(i)
           ${data.exprs.map(
               (expr, i) => `
           case(${i})
               cursor = savePoint
               ${expr}
               exit
           `
           )}
           case default
               call pegError()
           end select
       end do
`;

/**
*
* @param {{
*  exprs: string[]
*  startingRule: boolean
*  resultExpr: string
* }} data
* @returns
*/
export const union = (data) => `
               ${data.exprs.join('\n')}
               ${data.startingRule ? 'if (.not. acceptEOF()) cycle' : ''}
               ${data.resultExpr}
`;

/**
*
* @param {{
*  expr: string;
*  destination: string
*  quantifier?: string;
* }} data
* @returns
*/
export const strExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) cycle
                ${data.destination} = consumeInput()
        `;
    }
    switch (data.quantifier) {
        case '+':
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) cycle
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case '*':
            return `
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case '?':  
            return `
                lexemeStart = cursor
                if (${data.expr}) then
                    ${data.destination} = consumeInput()
                else
                    ${data.destination} = ""  ! Aceptar 0 coincidencias
                end if
                ${data.destination} = consumeInput()
            `;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
 };

/**
*
* @param {{
*  exprs: string[];
* }} data
* @returns
*/
export const strResultExpr = (data) => `
               res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')}
`;

/**
*
* @param {{
*  fnId: string;
*  exprs: string[];
* }} data
* @returns
*/
export const fnResultExpr = (data) => `
               res = ${data.fnId}(${data.exprs.join(', ')})
`;

/**
*
* @param {{
*  ruleId: string;
*  choice: number
*  signature: string[];
*  returnType: string;
*  paramDeclarations: string[];
*  code: string;
* }} data
* @returns
*/
export const action = (data) => {
   const signature = data.signature.join(', ');
   return `
   function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
       ${data.paramDeclarations.join('\n')}
       ${data.returnType} :: res
       ${data.code}
   end function peg_${data.ruleId}_f${data.choice}
   `;
};







/**
 * @param {{
*  code: string;
*  returnType: string;
*  params: object;
* }} data
* @returns {string}
*/
export const assertionPredicate = ({ code, returnType, params }) => `
      ! Guardar posición actual
      savePoint = cursor
      
      ! Ejecutar predicado
      ${code}
      
      if (result) then
          ! Si el predicado es verdadero, restaurar posición y continuar
          cursor = savePoint
      else
          ! Si el predicado es falso, restaurar posición y fallar
          cursor = savePoint
          cycle
      end if
`;

/**
* @param {{
*  expr: string;
* }} data
* @returns {string}
*/
export const assertionExpression = ({ expr }) => `
        ! Guardar posición actual
        savePoint = cursor
        
        ! Intentar la expresión
        if (${expr}) then
            ! Si la expresión coincide, restaurar posición y continuar
            
        else
            ! Si la expresión no coincide, restaurar posición y fallar
            cursor = savePoint
            cycle
        end if
`;

/**
* @param {{
    *  expr: string;
    * }} data
    * @returns {string}
    */
    export const assertionNegExpression = ({ expr }) => `
            if(${expr}) then
                print *, "ERROR: SE ENCONTRO ASERCION NEGATIVA"
                call exit(1)
            end if
    `;



// /**
//  * @param {{
// *  code: string;
// *  returnType: string;
// *  params: object;
// * }} data
// * @returns {string}
// */
// export const negAssertionPredicate = ({ code, returnType, params }) => `
//       ! Guardar posición actual
//       savePoint = cursor
      
//       ! Ejecutar predicado
//       ${code}
      
//       if (.not. result) then
//           ! Si el predicado es falso, restaurar posición y continuar
//           cursor = savePoint
//       else
//           ! Si el predicado es verdadero, restaurar posición y fallar
//           cursor = savePoint
//           cycle
//       end if
// `;

// /**
// * @param {{
// *  expr: string;
// * }} data
// * @returns {string}
// */
// export const negAssertionExpression = ({ expr }) => `
//       ! Guardar posición actual
//       savePoint = cursor
      
//       ! Intentar la expresión
//       if (.not. ${expr}) then
//           ! Si la expresión no coincide, restaurar posición y continuar
//           cursor = savePoint
//       else
//           ! Si la expresión coincide, restaurar posición y fallar
//           cursor = savePoint
//           cycle
//       end if
// `;