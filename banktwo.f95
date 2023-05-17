
program Simbank
implicit none

real, dimension(1000) :: saldosClientes
integer::cliente, i, dia
real::Iusu!Interes usuario
real::Imor !Interes moratorio
real::saldo !el saldo que va a tener el banco
real::monretprom!monto de retiro promedio
real::desvret!desviacion de retiro
character(len=1)::DistProbRet   !distribuciónretiro
real::senretiro          !
real::mondepprom!monto de deposito promedio
real::desvdep!desviacion de deposito
character(len=1)::DistProbDep  !distribución deposito
real::sendep, a
real::mondeprestamo!monto de prestamo
character(len=1)::DistProbPest   !distribucion de prestamo
real::senprest  !
real::desvdeprest
real::reservas
real::sumclientes
real::deuda
integer::dias,error_escritura, opcion

character(len=20)::nomodokumento,nomodokumento1
cliente=1000
!inicializa las variables
call system("color 0a")
call init(dias,saldo,monretprom,desvret,DistProbRet,senretiro, &  
    mondepprom,DESVDEP,DistProbDep,sendep,&
    mondeprestamo,desvdeprest,DistProbPest,senprest,Iusu,Imor,reservas)

    do i=1, cliente
    SaldosClientes(i)=0.0
    call random_number(a)
    SaldosClientes(i)=a*1000.0
    end do

!!Menu1
call random_seed()
200 write(*, *) "Valores actuales de las variables:"
    write(*, *) "1)dias de simulacion = ", dias
    write(*, *) "2)saldo del banco= ", saldo
    write(*, *) "3)Monto de retiro promedio = ", monretprom
    write(*, *) "4)Desviacion de retiro = ", desvret
    write(*, *) "5)Distribución de probabilidad de retiro = ", DistProbRet
    write(*, *) "6)Sensibilidad de retiro = ", senretiro
    write(*, *) "7)Monto de deposito promedio = ", mondepprom
    write(*, *) "8)Desviacion de deposito = ", desvdep
    write(*, *) "9)Distribución de probabilidad de deposito = ", DistProbDep
    write(*, *) "10)Sensibilidad de deposito = ", sendep
    write(*, *) "11)Monto de prestamo promedio = ", mondeprestamo
    write(*, *) "12)Desviacion de prestamo = ", desvdeprest
    write(*, *) "13)Distribución de probabilidad de prestamo = ", DistProbPest
    write(*, *) "14)sensibilidad de prestamo = ", senprest
    write(*, *) "15)tasa de interes diaria del usuario = ", Iusu
    write(*, *) "16)tasa de interes diaria morosa = ", Imor
    write(*,*)  "17)salir y simular"
    read(*, *) opcion
        if (opcion==1)then
            write(*, *) "Ingrese el nuevo valor de 'dias': "
            read(*, *) dias
            go to 200
          else if(opcion==2)then
            write(*, *) "Ingrese el nuevo valor de 'saldo': "
            read(*, *) saldo
            go to 200
          else if(opcion==3)then
            write(*, *) "Ingrese el nuevo valor de 'monretprom': "
            read(*, *) monretprom
            go to 200
          else if(opcion==4)then
            write(*, *) "Ingrese el nuevo valor de 'desvret': "
            read(*, *) desvret
            go to 200
          else if(opcion==5)then
            write(*, *) "Ingrese el nuevo valor de 'DistProbRet': "
            read(*, *) DistProbRet
            go to 200
          else if(opcion==6)then
            write(*, *) "Ingrese el nuevo valor de 'senretiro': "
            read(*, *) senretiro
            go to 200
          else if(opcion==7)then
            write(*, *) "Ingrese el nuevo valor de 'mondepprom': "
            read(*, *) mondepprom
            go to 200
          else if(opcion==8)then
            write(*, *) "Ingrese el nuevo valor de 'desvdep': "
            read(*, *) desvdep
            go to 200
          else if(opcion==9)then
            write(*, *) "Ingrese el nuevo valor de 'DistProbDep': "
            read(*, *) DistProbDep
            go to 200
          else if(opcion==10)then
            write(*, *) "Ingrese el nuevo valor de 'sendep': "
            read(*, *) sendep
            go to 200
          else if(opcion==11)then
            write(*, *) "Ingrese el nuevo valor de 'mondeprestamo': "
            read(*, *) mondeprestamo
            go to 200
          else if(opcion==12)then
            write(*, *) "Ingrese el nuevo valor de 'desvprest': "
            read(*, *) desvdeprest
            go to 200
          else if(opcion==13)then
            write(*, *) "Ingrese el nuevo valor de 'DistProbPest': "
            read(*, *) DistProbPest
            go to 200
          else if(opcion==14)then
            write(*, *) "Ingrese el nuevo valor de 'senprest': "
            read(*, *) senprest
            go to 200
          else if(opcion==15)then
            write(*, *) "Ingrese el nuevo valor de 'Iusu': "
            read(*, *) Iusu
            go to 200
          else if(opcion==16)then
            write(*, *) "Ingrese el nuevo valor de 'Imor': "
            read(*, *) Imor
            go to 200
          else if(opcion==17)then
           write(*,*)""
          else
            write(*, *) "Número inválido. Intente nuevamente."
        end if
    



    
!abre el documento donde vas a guardar la informacion para graficarla
1310    write(*,*) "Escriba como quiere guardar su documento"
		read(*,*) nomodokumento
    	nomodokumento1=nomodokumento//".dat"	
       OPEN (100,FILE=nomodokumento1,status='new',IOSTAT=error_escritura)
       
       Write(100,*,err=23)"          -@IsraelGarcia 2023-            "
23     if(error_escritura>0)then
         Write(*,*)"Error en el nombre del archivo, porfavor, elija otro nombre"
         go to 1310
       end if


       
do dia = 1, dias
    ! Simular un depósito si se cumple la probabilidad
    call random_number(a)
    if (a < sendep) then
      call deposito(mondepprom,desvdep,DistProbDep,SaldosClientes,cliente, saldo)
      ! Agregar una transacción a la tabla
    end if

    
    !! Simular un retiro
    
    call random_number(a)

    if (a<senretiro) then
      call retiro(monretprom,desvret,distprobret,SaldosClientes,cliente,saldo)
      
      ! Agregar una transacción a la tabla
    end if
    
    !simular un prestamo
    call random_number(a)

    if (a<senprest) then

      call prestamo(mondeprestamo,desvdeprest,DistProbPest,SaldosClientes,cliente, saldo, reservas)

    end if





!!pagoIntereses
    do i=1, cliente
      if(SaldosClientes(i)<0)then
        SaldosClientes(i)=SaldosClientes(i)*(1+Imor)
      else if(SaldosClientes(i)<0) then
        SaldosClientes(i)=SaldosClientes(i)*(1+Iusu) 
      end if     
    end do
    sumclientes=0.0
    deuda=0.0
    do i=1, cliente
        sumclientes=sumclientes+saldosclientes(i)
        if (saldosclientes(i)<0)then
          deuda=deuda-saldosclientes(i)
        end if
    end do
    write(100,*)Saldo, sumclientes, deuda, Saldo+deuda, deuda/(saldo+deuda)
end do

   

end program




subroutine init(dias,saldo,monretprom,desvret,DistProbRet,senretiro, &  
    mondepprom,DESVDEP,DistProbDep,sendep,&
    mondeprestamo,desvdeprest,DistProbPest,senprest,Iusu,Imor,reservas)
implicit none
character, intent(inout)::DistProbRet,DistProbDep,DistProbPest
real, intent(inout)::Iusu!Interes usuario
real, intent(inout)::Imor !Interes moratorio
real, intent(inout)::saldo !el saldo que va a tener el banco
real, intent(inout)::monretprom!monto de retiro promedio
real, intent(inout)::desvret!desviacion de retiro
real, intent(inout)::senretiro          !
real, intent(inout)::mondepprom!monto de deposito promedio
real, intent(inout)::sendep
real, intent(inout)::mondeprestamo!monto de prestamo
real, intent(inout)::desvdeprest!desviacion de pretamo
real, intent(inout)::senprest, reservas,DESVDEP  !
integer, intent(inout)::dias

    dias=1000!dias que va a durar la simulacion
    saldo=1000000.0!el saldo que va a tener el banco
    monretprom=500.0!monto de retiro promedio
    desvret=300.0!desviacion de retiro
    DistProbRet="n"   !distribuciónretiro
    senretiro=0.4            !
    mondepprom=10000.0!monto de deposito promedio
    desvdep=300.0!desviacion de deposito
    DistProbDep="n"   !distribución deposito
    sendep=0.9                 !
    mondeprestamo=500.0!monto de prestamo
    desvdeprest=30.0!desviacion de prestamo
    DistProbPest="n"   !distribucion de prestamo
    senprest=1.0               !
    Iusu=0.0001         !Interes usuario diario
    Imor=0.003         !Interes moratorio diario
    reservas=1.0

end subroutine



subroutine deposito(mondepprom,desvdep,DistProbDep,SaldosClientes,cliente, saldo)
implicit none

    real, intent(inout)::saldo, desvdep,mondepprom
    integer, intent(inout)::cliente
    integer:: kliente
    real::k,u1,random_normal,random_exponential, cantdep



    character(len=1)::distprobdep
    real, dimension(1000) :: saldosClientes
        if(distprobdep=="n")then
          Cantdep=random_normal(mondepprom,desvdep)
          !cantidad de deposito
        else if(distprobdep=="c")then
          call random_number(u1)
          Cantdep=mondepprom*u1
        else if(distprobdep=="e")then
          Cantdep=random_exponential(1/mondepprom)
        end if

        call random_number(K)
        kliente=int((cliente-1.0)*K)+1
        SaldosClientes(kliente)=SaldosClientes(kliente)+Cantdep
        Saldo=saldo+Cantdep
end subroutine





subroutine retiro(monretprom,desvret,distprobret,SaldosClientes,cliente, saldo)
implicit none       
    character(len=1)::distprobret
    real, intent(inout), dimension(1000) :: saldosClientes
    real, intent(inout)::saldo, desvret,monretprom
    integer, intent(inout)::cliente
    integer:: kliente
    real::k, cantret,random_exponential,u1,random_normal
        if(distprobret=="n")then
          Cantret=random_normal(monretprom,desvret)
          !cantidad de deposito
        else if(distprobret=="c")then
          call random_number(u1)
          Cantret=monretprom*u1
        else if(distprobret=="e")then
          Cantret=random_exponential(1.0/monretprom)
        end if
        call random_number(K)
        kliente=int((cliente-1.0)*K)+1
        SaldosClientes(kliente)=SaldosClientes(kliente)-Cantret
        Saldo=saldo-Cantret
end subroutine




subroutine prestamo(mondeprestamo,desvdeprest,DistProbPest,SaldosClientes,cliente, saldo, reservas)
implicit none

    real, intent(inout)::saldo,desvdeprest,mondeprestamo, reservas
    integer, intent(inout)::cliente
    integer:: kliente
    real::k,u1,random_normal,random_exponential, cantdep
    character(len=1)::DistProbPest
    real, dimension(1000) :: saldosClientes
        if(DistProbPest=="n")then
          Cantdep=random_normal(mondeprestamo,desvdeprest)
  
          !cantidad de deposito
        else if(DistProbPest=="c")then
          call random_number(u1)
          Cantdep=mondeprestamo*u1
        else if(DistProbPest=="e")then
          Cantdep=random_exponential(1/mondeprestamo)
        end if
        if (Cantdep<reservas*saldo)then
            call random_number(K)
            kliente=int((cliente-1.0)*K)+1
       
            SaldosClientes(kliente)=SaldosClientes(kliente)-Cantdep
         
            Saldo=Saldo-Cantdep
        end if
end subroutine



!!11111111111111111111111111111111111111111111111111111111
!para distribuciones
! n normal
! c constante
! e exponencial
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


function random_normal(mu, sigma) result(numero)
    implicit none
    real:: numero, mu, sigma
    real:: u1, u2, z
    real, parameter :: pi = 4.0*atan(1.0)

    call random_number(u1)
    call random_number(u2)

    z = sqrt(-2.0 * log(u1)) * cos(2.0 * pi * u2)
    numero = mu + sigma * z
end function random_normal



function random_exponential(lambda) result(numero)
    implicit none
    real:: numero, lambda
    real:: u

    call random_number(u)
    numero = -log(1.0 - u) / lambda
 end function random_exponential