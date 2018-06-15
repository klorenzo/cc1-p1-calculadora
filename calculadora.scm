(load "operaciones.scm")
(load "validaciones.scm")

(clear)

(newline)
(newline)

(display "=====================================================================================")(newline)
(display "::: Bienvenido a La Calculadora Virtual :::::::::::::::::::::::::::::::::::::::::::::")(newline)
(display "=====================================================================================")(newline)
(display "                                                                                     ")(newline)
(display " Integrantes:                                                                        ")(newline)
(display " - Kevin Lorenzo                                                                     ")(newline)
(display "                                                                                     ")(newline)
(display "=====================================================================================")(newline)

(define expresion)
(define longitudDeLaExpresion)
(define resultado)

(define (menu)

	(display "\ncalculadora >> ")

	(set! expresion (read-line))
	(set! longitudDeLaExpresion (string-length expresion))

	(if (not (= longitudDeLaExpresion 0))
		(if (string=? expresion "quit")
			(begin
				(display "Saliendo...\n")
				(display "Gracias por usar nuestra calculadora.\n")
			)
			(if (sintaxis1 expresion)
				(begin
					(display (string RESPUESTA expresion))
					(menu)
				)
				(begin
					(set! resultado (sintaxis2 expresion))
					(if (not (null? resultado))
						(begin
							(display (string RESPUESTA (operacionSimple1 resultado)))
							(menu)
						)
						(begin
							(set! resultado (sintaxis3 expresion))
							(if (not (null? resultado ))
								(begin
									(display (string RESPUESTA (operacionSimple2 resultado)))
									(menu)
								)
								(begin
									(set! resultado (sintaxis4 expresion))
									(if (or (number? (string->number resultado)) (string=? resultado DIVISION_BY_ZERO)
											(string=? resultado NEGATIVE_SQUARE_ROOT))
										(begin
											(display (string RESPUESTA resultado))
											(menu)
										)
										(begin
											(display (string RESPUESTA NOT_VALID_EXPRESSSION))
											(menu)
										)
									)
								)
							)
						)
					)
				)
			)
		)
		(begin
			(display (string RESPUESTA NOT_VALID_EXPRESSSION))
			(menu)
		)
	)

)(menu)

(newline)
(newline)

; (load "calculadora.scm")
