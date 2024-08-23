module inventario_mod

    implicit none
    ! Definimos constantes para los tamaños
    integer, parameter :: max_equipos = 1000000
    integer, parameter :: max_ubicaciones = 1000000

    ! Estructura para almacenar información del equipo
    type :: equipo
        character(len=50) :: nombre
        integer :: cantidad
        real :: precio_unitario
        character(len=50) :: ubicacion
    end type equipo

    ! Arreglo para almacenar los equipos en el inventario
    type(equipo), dimension(max_equipos) :: inventario
    integer :: num_equipos = 0

contains

    ! Subrutina para agregar un equipo al inventario
    subroutine agregar_equipo(nombre, cantidad, precio_unitario, ubicacion)
        implicit none
        character(len=*), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        real, intent(in) :: precio_unitario
        integer :: i
        logical :: encontrado

        ! Inicializamos la variable encontrado en falso
        encontrado = .false.

        ! Buscamos si el equipo ya existe en el inventario
        do i = 1, num_equipos
            if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
                print *, "El equipo ya existe en el inventario, no se agregará de nuevo."
                encontrado = .true.
                exit
            end if
        end do

        ! Si no se encontró el equipo, lo agregamos
        if (.not. encontrado) then
            if (num_equipos < max_equipos) then
                num_equipos = num_equipos + 1
                inventario(num_equipos)%nombre = nombre
                inventario(num_equipos)%cantidad = cantidad
                inventario(num_equipos)%precio_unitario = precio_unitario
                inventario(num_equipos)%ubicacion = ubicacion   
                print *, "Equipo agregado: ", trim(nombre), "   Cantidad: ", cantidad, "    Precio Unitario: ", precio_unitario, "Ubicacion: ", trim(ubicacion)
            else
                print *, "Error: Inventario lleno, no se puede agregar más equipos."
            end if
        end if

    end subroutine agregar_equipo


    ! Subrutina para cargar el inventario inicial
    subroutine cargar_inventario(nombre_archivo)
        implicit none
        character(len=*), intent(in) :: nombre_archivo
        character(len=100) :: linea, nombre, ubicacion
        integer :: cantidad, ios
        real :: precio_unitario
        integer :: pos1, pos2, pos3
    
        ! Abrimos el archivo en modo lectura
        open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo:", nombre_archivo
            return
        end if
    
        ! Leemos el archivo línea por línea
        do while (.true.)
            read(10, '(A)', iostat=ios) linea
            if (ios /= 0) exit  ! Salimos del bucle si alcanzamos el final del archivo
    
            ! Buscamos las posiciones de los separadores (;)
            pos1 = index(linea, ';')
            pos2 = index(linea(pos1+1:), ';') + pos1
            pos3 = index(linea(pos2+1:), ';') + pos2
    
            ! Extraemos los valores
            nombre = linea(14:pos1-1)
            read(linea(pos1+1:pos2-1), *) cantidad
            read(linea(pos2+1:pos3-1), *) precio_unitario
            ubicacion = linea(pos3+1:)
    
            ! Llamamos a la subrutina agregar_equipo para almacenar el equipo en el inventario
            call agregar_equipo(nombre, cantidad, precio_unitario, ubicacion)
        end do
    
        ! Cerramos el archivo
        close(10)
    
        print *, "Inventario inicial cargado correctamente."
    
    end subroutine cargar_inventario    

    subroutine agregar_stock(nombre, cantidad, ubicacion)
        implicit none
        character(len=*), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        integer :: i
        logical :: encontrado
    
        ! Inicializamos la variable encontrado en FALSO
        encontrado = .false.
    
        ! Buscamos el equipo en el inventario
        do i = 1, num_equipos
            ! Imprimimos para depuración
            ! print *, "Comparando con inventario: ", trim(inventario(i)%nombre), " en ", trim(inventario(i)%ubicacion)
            ! print *, "Datos actuales: Nombre: ", trim(nombre), " | Ubicación: ", trim(ubicacion)
    
            if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
                ! Actualizamos la cantidad
                inventario(i)%cantidad = inventario(i)%cantidad + cantidad
                print *, "Se agregaron: ", cantidad, " al producto: ", nombre, " en la ubicacion: ", ubicacion
                encontrado = .true.
                exit
            end if
        end do
    
        ! Si no se encontró el equipo, mostrar un mensaje de error
        if (.not. encontrado) then
            print *, "Error: El equipo ", trim(nombre), " en la ubicacion ", trim(ubicacion), " no se puede agregar ya que no existe en el inventario."
        end if
    
    end subroutine agregar_stock
    
    

    subroutine eliminar_equipo(nombre, cantidad, ubicacion)
        implicit none
        character(len=*), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        integer :: i
        logical :: encontrado
    
        ! Inicializamos la variable encontrado en falso
        encontrado = .false.
    
        ! Buscamos el equipo en el inventario
        do i = 1, num_equipos
            if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
                encontrado = .true.
    
                if (inventario(i)%cantidad > cantidad .or. inventario(i)%cantidad == cantidad) then
                    ! Si la cantidad en el inventario es mayor, reducimos la cantidad
                    inventario(i)%cantidad = inventario(i)%cantidad - cantidad
                    print *, "Se eliminaron: ", cantidad, " del producto ", nombre, " en la ubicacion: ", ubicacion
                else if (inventario(i)%cantidad < cantidad) then
                    ! Si la cantidad es igual o menor, eliminamos todo el equipo
                    print *, "No es posible eliminar esta cantidad del producto: ", nombre, " en la ubicacion: ", ubicacion, " ya que es mayor a la cantidad en el inventario."
                    
                end if
    
                exit
            end if
        end do
    
        ! Si no se encontró el equipo, mostrar un mensaje de error
        if (.not. encontrado) then
            print *, "Error: El equipo ", nombre, " en la ubicacion ", ubicacion, " no se puede eliminar ya que no existe en el inventario."
        end if
    
    end subroutine eliminar_equipo
    

    subroutine procesar_movimientos(nombre_archivo)
        implicit none
        character(len=*), intent(in) :: nombre_archivo
        character(len=100) :: linea, comando
        character(len=50) :: nombre, ubicacion
        integer :: cantidad, ios, pos1, pos2 ! las variables pos son para encontrar los separadores (;)
    
        ! Abrir el archivo de movimientos
        open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo:", nombre_archivo
            return
        end if
    
        ! Leer cada línea del archivo de movimientos
        do
            read(10, '(A)', iostat=ios) linea
            if (ios /= 0) exit  ! Fin de archivo o error de lectura
    
            ! Extraer el comando, nombre, cantidad y ubicación de la línea
            pos1 = index(linea, ' ')
            comando = trim(adjustl(linea(1:pos1-1)))
            linea = trim(adjustl(linea(pos1+1:)))
    
            ! Encontrar las posiciones de los separadores (;)
            pos1 = index(linea, ';')
            pos2 = index(linea(pos1+1:), ';') + pos1
    
            ! Extraer nombre, cantidad y ubicación
            nombre = trim(adjustl(linea(1:pos1-1)))
            read(linea(pos1+1:pos2-1), *) cantidad
            ubicacion = trim(adjustl(linea(pos2+1:)))
    
            if (trim(comando) == "agregar_stock") then
                ! Procesar un comando de agregar stock
                call agregar_stock(trim(nombre), cantidad, trim(ubicacion))
    
            else if (trim(comando) == "eliminar_equipo") then
                ! Procesar un comando de quitar stock
                call eliminar_equipo(trim(nombre), cantidad, trim(ubicacion))     
            else
                print *, "Comando desconocido en el archivo de movimientos:", trim(comando)
            end if
        end do
    
        ! Cerrar el archivo de movimientos
        close(10)
    
    end subroutine procesar_movimientos    
    

    subroutine generar_informe()
        implicit none
        integer :: i
        real :: valor_total
        character(len=50) :: nombre_archivo = 'informe.txt'
        integer :: unit_number
    
        ! Abrimos el archivo para escribir el informe
        open(newunit=unit_number, file=nombre_archivo, status='replace', action='write')
    
        ! Escribimos la cabecera del informe
        write(unit_number, '(A)') 'Informe de Inventario:'
        write(unit_number, '(A)') '---------------------------------------------------------------------------'
        write(unit_number, '(A15, A10, A18, A18, A)') 'Equipo', 'Cantidad', 'Precio Unitario', 'Valor Total', '     Ubicación'
        write(unit_number, '(A)') '---------------------------------------------------------------------------'
    
        ! Escribimos cada equipo en el informe
        do i = 1, num_equipos
            valor_total = inventario(i)%cantidad * inventario(i)%precio_unitario
            write(unit_number, '(A15, 2X, I8, 2X, A1, F8.2, 2X, A1, F12.2, 2X, A15)') &
                trim(inventario(i)%nombre), &
                inventario(i)%cantidad, &
                'Q', inventario(i)%precio_unitario, &
                'Q', valor_total, &
                trim(inventario(i)%ubicacion)
        end do
    
        ! Escribimos una línea final de separación
        write(unit_number, '(A)') '---------------------------------------------------------------------------'
    
        ! Cerramos el archivo
        close(unit_number)
    
        print *, 'Informe generado exitosamente en ', trim(nombre_archivo)
    
    end subroutine generar_informe
    
    


end module inventario_mod
