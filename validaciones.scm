(load "constantes.scm")

; sintaxis1: Función utilizada para verificar si el valor introducido es un número.
; Parámetros: expresion ( Valor ingresado por el usuario, el cual se verificará ).

(define (sintaxis1 expresion)
	(define resultado #f)
	(if (number? (string->number expresion))
		(set! resultado #t)
	)
	resultado
)

; sintaxis2: Función utilizada para verificar si el valor introducido es una expresión de la forma: (operacion numero)
; Parámetros: expresion ( Valor ingresado por el usuario, el cual se verificará ).

(define (sintaxis2 expresion)

	(define resultado (list))

	(define operacion)
	(define operando)

	(define longitudDeLaExpresion (string-length expresion))
	(define listaDeEspacios (string-search-all " " expresion))

	(if (and (char=? (string-ref expresion 0) #\( ) (char=? (string-ref expresion (- longitudDeLaExpresion 1)) #\) ) )
		(if (= (length listaDeEspacios) 1)
			(begin
				
				(set! operacion (substring expresion 1 (first listaDeEspacios)))
				(set! operando (substring expresion (+ (first listaDeEspacios) 1) (- longitudDeLaExpresion 1)))

				(if (number? (string->number operando))
					(if (or (string=? operacion SQUARE_ROOT) (string=? operacion SQUARE_SIMPLE) (string=? operacion SENO) (string=? operacion COSENO) 
						(string=? operacion TANGENTE) (string=? operacion FACTORIAL))
						(set! resultado (list operacion operando))
					)
				)

			)
		)
	)

	resultado

)

; sintaxis3: Función utilizada para verificar si el valor introducido es una expresión de la forma: (numero operador numero)
; Parámetros: expresion ( Valor ingresado por el usuario, el cual se verificará ).

(define (sintaxis3 expresion)

	(define resultado (list))

	(define operando1)
	(define operando2)
	(define signo)

	(define longitudDeLaExpresion (string-length expresion))
	(define listaDeEspacios (string-search-all " " expresion))

	(if (and (char=? (string-ref expresion 0) #\( ) (char=? (string-ref expresion (- longitudDeLaExpresion 1)) #\) ) )
		(if (= (length listaDeEspacios) 2)
			(begin

				(set! operando1 (substring expresion 1 (first listaDeEspacios)))
				(set! operando2 (substring expresion (+ (second listaDeEspacios) 1) (- longitudDeLaExpresion 1)))
				(set! signo (substring expresion (+ (first listaDeEspacios) 1) (second listaDeEspacios)))

				(if (and (number? (string->number operando1)) (number? (string->number operando2)))
					(if (or (string=? signo SUMA) (string=? signo RESTA) (string=? signo MULTIPLICACION) (string=? signo DIVISION) (string=? signo RESIDUO) 
						(string=? signo COCIENTE))
						(set! resultado (list operando1 operando2 signo))
					)
				)

			)
		)
	)

	resultado

)

; sintaxis3: Función utilizada para verificar si el valor introducido es una expresión compuesta.
; Parámetros: expresion ( Valor ingresado por el usuario, el cual se verificará ).

(define (sintaxis4 expresionCompuesta)

	(define longitudDeLaExpresionCompuesta)
	(define inicioExpresionSimple #f)
	(define finalExpresionSimple #f)
	(define expresionSimple)

	(define resultadoExpresionSimple "")

	(define (ciclo)
		(if (not (number? (string->number expresionCompuesta)))
			(begin

				(set! longitudDeLaExpresionCompuesta (string-length expresionCompuesta))
				(set! inicioExpresionSimple (string-search-backward "(" expresionCompuesta))

				(if (integer? inicioExpresionSimple)
					(begin
						(set! inicioExpresionSimple (- inicioExpresionSimple 1))
						(set! finalExpresionSimple (substring-search-forward ")" expresionCompuesta inicioExpresionSimple longitudDeLaExpresionCompuesta))
					)
				)
				
				(if (and (number? inicioExpresionSimple) (number? finalExpresionSimple))
					(begin

						(set! finalExpresionSimple (+ finalExpresionSimple 1))
						(set! expresionSimple (substring expresionCompuesta inicioExpresionSimple finalExpresionSimple))

						(cond
							((not (null? (sintaxis2 expresionSimple)))
								(set! resultadoExpresionSimple (string (operacionSimple1 (sintaxis2 expresionSimple))))
							)
							((not (null? (sintaxis3 expresionSimple)))
								(set! resultadoExpresionSimple (string (operacionSimple2 (sintaxis3 expresionSimple))))
							)
							((string=? (substring expresionSimple 1 (- (string-length expresionSimple) 1)) DIVISION_BY_ZERO)
								(set! resultadoExpresionSimple DIVISION_BY_ZERO)
							)
							((string=? (substring expresionSimple 1 (- (string-length expresionSimple) 1)) NEGATIVE_SQUARE_ROOT)
								(set! resultadoExpresionSimple NEGATIVE_SQUARE_ROOT)
							)
							(else
								(if (not (number? (string->number (substring expresionSimple 1 (- (string-length expresionSimple) 1) ))))
									(set! resultadoExpresionSimple "")
								)
							)
						)

						(set! expresionCompuesta (string (substring expresionCompuesta 0 inicioExpresionSimple) resultadoExpresionSimple 
							(substring expresionCompuesta finalExpresionSimple (string-length expresionCompuesta)) ))
						
						(if (not (string=? resultadoExpresionSimple ""))
							(ciclo)
						)

					)
				)

			)
		)
	)
	
	(if (not (or (number? (string-search-forward COCIENTE expresionCompuesta)) (number? (string-search-forward RESIDUO expresionCompuesta))
			(number? (string-search-forward FACTORIAL expresionCompuesta))))
		(ciclo)
	)

	expresionCompuesta

)