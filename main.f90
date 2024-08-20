PROGRAM prueba_inventario
    USE inventario_mod
    implicit none

    integer :: opcion


    do
        print *, "===================================================="
        print *, "Practica 1: Lenguajes Formales y de Programacion"
        print *, "===================================================="
        print *, "# Sistema de inventario:"
        print *, ""
        print *, "1. Cargar Inventario inicial"
        print *, "2. Cargar Instrucciones de movimientos"
        print *, "3. Crear Informe de inventario"
        print *, "4. Salir"
        print *, ""
        print *, "Ingrese una opcion: "
        read *, opcion

        select case (opcion)
        case (1)
            ! Leer inventario inicial
            print *, "Cargando Inventario inicial..."
            call sleep(1)
            call cargar_inventario("inventario.inv")
        case (2)
            ! Procesar movimientos
            print *, "Cargando Instrucciones de movimientos..."
            call sleep(1)
            call procesar_movimientos("instrucciones.mov")
        case (3)
            ! Generar informe
            print *, "Creando Informe de inventario..."
            call sleep(1)
            call generar_informe()
        case (4)
            exit
        case default
            print *, 'Opción no válida.'
        end select
    end do

END PROGRAM prueba_inventario
