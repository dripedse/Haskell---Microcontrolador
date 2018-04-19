nop microprocesador = microprocesador { programCounter=programCounter microprocesador + 1}
lodv value microprocesador = microprocesador { acumuladorA = value, programCounter = programCounter microprocesador + 1}
swap microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador, programCounter = programCounter microprocesador + 1 }
add microprocesador = microprocesador { acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0, programCounter = programCounter microprocesador + 1}


