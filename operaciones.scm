(load "constantes.scm")

; calcularFactorial: Función utilizada para calcular el factorial de un número.
; Parámetros: numero ( Número al que se le calculará su factorial ).

(define (calcularFactorial numero)

	(define resultado numero)

	(define (ciclo)
		(if (>= numero 2)
			(begin
				(set! numero (- numero 1))
				(set! resultado (* resultado numero))
				(ciclo)
			)
		)
	)

	(if (= numero 0)
		(set! resultado 1)
		(ciclo)
	)

	resultado

)

; operacionSimple1: Función utilizada para calcular las siguientes operaciones: sqroot, sqr, sen, cos, tan y fact!.
; Parámetros: elementosDeLaExpresion ( Lista que contiene los elementos de la expresión ).

(define (operacionSimple1 elementosDeLaExpresion)

	(define resultado)

	(define operacion (first elementosDeLaExpresion))
	(define operando (string->number (second elementosDeLaExpresion)))

	(cond
		((string=? operacion SQUARE_ROOT)
			(if (< operando 0)
				(set! resultado NEGATIVE_SQUARE_ROOT)
				(set! resultado (sqrt operando))
			)
		)
		((string=? operacion SQUARE_SIMPLE)
			(set! resultado (square operando))
		)
		((string=? operacion SENO)
			(set! resultado (sin (/ (* operando PI) 180)))
		)
		((string=? operacion COSENO)
			(set! resultado (cos (/ (* operando PI) 180)))
		)
		((string=? operacion TANGENTE)
			(set! resultado (tan (/ (* operando PI) 180)))
		)
		((string=? operacion FACTORIAL)
			(if (>= operando 0)
				(set! resultado (calcularFactorial operando))
				(set! resultado IMPOSSIBLE_FACTORIAL)
			)
		)
	)

	resultado

)

; operacionSimple2: Función utilizada para calcular las siguiente operaciones: +, -, /, *, % y div.
; Parámetros: elementosDeLaExpresion ( Lista que contiene los elementos de la expresión ).

(define (operacionSimple2 elementosDeLaExpresion)

	(define resultado)

	(define operando1 (string->number (first elementosDeLaExpresion)))
	(define operando2 (string->number (second elementosDeLaExpresion)))
	(define signo (third elementosDeLaExpresion))

	(cond
		((string=? signo SUMA) 
			(set! resultado (+ operando1 operando2))
		)
		((string=? signo RESTA)
			(set! resultado (- operando1 operando2))
		)
		((string=? signo DIVISION)
			(if (= operando2 0)
				(set! resultado DIVISION_BY_ZERO)
				(set! resultado (* (/ operando1 operando2) 1.0))
			)
		)
		((string=? signo MULTIPLICACION) 
			(set! resultado (* operando1 operando2))
		)
		((string=? signo RESIDUO)
			(if (= operando2 0)
				(set! resultado DIVISION_BY_ZERO)
				(set! resultado (remainder operando1 operando2))
			)
		)
		((string=? signo COCIENTE)
			(if (= operando2 0)
				(set! resultado DIVISION_BY_ZERO)
				(set! resultado (quotient operando1 operando2))
			)
		)
	)

	resultado

)