C******************************************************************************                                                                              
C     FILE: mpi_latency.f                                                                                                                                    
C     DESCRIPTION:                                                                                                                                           
C     MPI Latency Timing Program - Fortran Version                                                                                                           
C     In this example code, an MPI communication timing test is performed.                                                                                   
C     MPI task 0 will send "reps" number of 1 byte messages to MPI task 1,                                                                                   
C     waiting for a reply between each rep. Before and after timings are made                                                                                
C     for each rep and an average calculated when completed.                                                                                                 
C     AUTHOR: Blaise Barney                                                                                                                                  
C     LAST REVISED: 03/20/08                                                                                                                                 
C*****************************************************************************                                                                               
C     Variable definitions:                                                                                                                                  
C     REPS                    =  number of samples per test                                                                                                  
C     tag                     =  MPI message tag parameter                                                                                                   
C     numtasks                =  number of MPI tasks                                                                                                         
C     rank                    =  my MPI task number                                                                                                          
C     dest, source            =  send/receive task designators                                                                                               
C     avgT                    =  average time per rep in microseconds                                                                                        
C     ierr                    =  return code                                                                                                                 
C     T1, T2                  =  start/end times per rep                                                                                                     
C     sumT                    =  sum of all reps times                                                                                                       
C     deltaT                  =  time for one rep                                                                                                            
C     msg                     =  buffer containing 1 byte message                                                                                            
C     status                  =  MPI receive routine parameter                                                                                               

      program latency
      include 'mpif.h'
      integer REPS,NREPS
      parameter(REPS = 1000)
      integer rank, numtasks,aux,i,j,status(MPI_STATUS_SIZE)
      real, allocatable :: timesf(:), nztimes(:,:), desvxf(:)
      integer, allocatable :: sourceidxf(:), destidxf(:) 
      integer tag,ierr,iunit
      real avgT,sumtotal
      integer dest,source,sizevec,p
      double precision T1, T2, sumT, deltaT
      real, allocatable :: times(:),desvaux(:),desv(:)
      integer, allocatable :: sourceidx(:)
      integer, allocatable :: destidx(:)
      character*12 :: filename
c      character msg
      real(8), allocatable :: msg(:)
      
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      
      aux=numtasks*numtasks-1
      allocate(nztimes(numtasks*(numtasks-1),4))
      allocate(timesf(0:aux),times(0:numtasks-1))
      allocate(sourceidxf(0:aux),sourceidx(0:numtasks-1))
      allocate(destidxf(0:aux),destidx(0:numtasks-1))
      allocate(desvaux(reps),desv(0:numtasks-1))  
      allocate(desvxf(0:aux))
c     point to point communication
      open(unit=50,file='testotal.TXT',status='unknown')
      
c      change size of the message 
      do 2000 p=0,25,5
      iunit=p
      write(filename,'(a,I4.4,a)')"TEST",iunit,".TXT"
      open(unit=iunit,file=filename,status="unknown")
      sizevec=2**(p)                      
      allocate(msg(sizevec))
      msg=1.d0
      !write(iunit,*) size(msg)
      times=0.d0; sourceidx=0.d0; destidx=0.d0
      timesf=0.d0; sourceidxf=0.d0; destidxf=0.d0
      desv=0.d0
C     POINT  TO POINT COMMUNICATION
      do 1000 j= 0, numtasks-1
       do 100 i= 0,numtasks-1
      
        if (i.eq.j)   then
c       nothing happens
        else
         tag=i
         if (rank .eq. j) then
           ! if (i-1.eq.j) then    
             !write(*,*) 'master',rank,'has started...'
           ! endif
c$$$        write(*,*) 'Beginning latency timing test. Number of reps=',REPS                                                                                   
c$$$        write(*,*) '***********************************************'                                                                                 
c$$$        write(*,*)'Rep#       T1                T2            deltaT'                                                                            
            !print*,i                  
            sumT = 0.0
            dest = i
            source = i
            NREPS = 0
            desvaux=0.d0
                !write(*,*) 'source master',source,'destination master',
c     &              dest  
            do 10 n = 1, REPS
               NREPS=NREPS+1
C     Get start time                                                                                                                                         
               T1 = MPI_WTIME()
               call MPI_SENDRECV(msg,1,MPI_REAL8,dest,tag,
     &              msg,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,
     &              status,ierr)           
 
C     Get ending time                                                                                                                                        
               T2 = MPI_WTIME()
               deltaT = T2 - T1

c$$$               if (NREPS.eq.500) then
c$$$                  write(*,9) n, T1, T2, deltaT
c$$$9                format(I4, F22.8,F22.8, F12.8)
c$$$                  NREPS=0
c$$$               end if

               sumT = sumT + deltaT
               desvaux(n)=deltaT
10         continue

           avgT = (sumT * 1000000) / REPS
           do 60 n= 1,reps
           desv(i)=desv(i)+(desvaux(n)*1000000-avgT)**2.d0
60         continue
           desv(i)=sqrt(desv(i)/(reps-1))
           times(i)=avgT
           sourceidx(i)=j
           destidx(i)=dest
C          Print final average from all round trips                                                                                                  
               
c$$$        write(*,*)'**********************************************'                                                                                
c$$$        write(*,*)'*** send=',dest, 'receive=', source                                                                                                                                     
c$$$        write(*,*)'*** Avg round trip time=', avgT, 'microseconds'                                                                                         
c$$$        write(*,*)'*** Avg one way latency=', avgT/2, 'microseconds'    
         
         endif                                                                             
         
         if (rank .eq. i) then                                                                                                                   
        ! print*,'test',rank          
          ! write(*,*)'task',rank,'has started...'                                                                                                         
            dest = j                                                                                                                                         
            source = j                                                                                                                               
           ! write(*,*) 'source',source,'destination',dest      
            do 20 n = 1, REPS                                                                                                                   
               call MPI_SENDRECV(msg,1,MPI_REAL8,source,tag,
     &              msg,1,MPI_REAL8,dest,tag,MPI_COMM_WORLD,
     &              status,ierr)                                                                                                      
                   
20         continue                                                                                                                               
                 
         endif                                             
         end if                                                                                                                                      
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)  
100   continue 
c      call MPI_BARRIER(MPI_COMM_WORLD,ierr)  
1000  continue
     

      call MPI_ALLGATHER(times,numtasks,MPI_INT,
     &     timesf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr) 
      call MPI_ALLGATHER(sourceidx,numtasks,MPI_INT,
     &     sourceidxf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr)
      call MPI_ALLGATHER(destidx,numtasks,MPI_INT,
     &     destidxf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr)    
       call MPI_ALLGATHER(desv,numtasks,MPI_INT,
     &     desvxf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr)   
       


       IF (rank.eq.master) then
c       print*,'lala'
c       print*,size(timesf)
c       print*,timesf
!      end if
       aux=1
       do 40 i=1,numtasks*numtasks
          if(sourceidxf(i-1).ne.destidxf(i-1)) then
          nztimes(aux,1)=sourceidxf(i-1)
          nztimes(aux,2)=destidxf(i-1)
          nztimes(aux,3)=timesf(i-1)
          nztimes(aux,4)=desvxf(i-1)
          aux=aux+1
       end if
40     continue
       do 50 i=1,numtasks*(numtasks-1)
       write(iunit,*)(nztimes(i,j),j=1,4),i 
       if (i.eq.1) then
c      nothing happend
       else if (nztimes(i+1,1).ne.nztimes(i,1)) then 
c      create an space for gnuplot
       write(iunit,*)''
       end if           
50     continue
       sumtotal=sum(nztimes(:,3))/(numtasks*(numtasks-1))    
       write(50,*)p,sizevec,sumtotal
       end if
       close(unit=iunit)
       deallocate(msg)
       call MPI_BARRIER(MPI_COMM_WORLD,ierr)  
2000   continue  

          call MPI_FINALIZE(ierr)                                                                                                         







        end        



     

     
