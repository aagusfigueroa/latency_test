C******************************************************************************
C FILE: mpi_latency1.f
C DESCRIPTION:  
C   MPI Latency Timing Program - Fortran Version
C   In this example code, an MPI communication timing test is performed.
C   MPI task 0 will send "reps" number of 1 byte messages to MPI task 1,
C   waiting for a reply between each rep. Before and after timings are made
C   for each rep and an average calculated when completed.
C AUTHOR: Blaise Barney
C LAST REVISED: 03/20/08
C*****************************************************************************
C Variable definitions:
C REPS                    =  number of samples per test 
C tag                     =  MPI message tag parameter 
C numtasks                =  number of MPI tasks 
C rank                    =  my MPI task number 
C dest, source            =  send/receive task designators 
C avgT                    =  average time per rep in microseconds
C ierr                    =  return code 
C T1, T2                  =  start/end times per rep 
C sumT                    =  sum of all reps times 
C deltaT                  =  time for one rep 
C msg                     =  buffer containing 1 byte message 
C status                  =  MPI receive routine parameter

      program latency1
      include 'mpif.h'

      integer REPS,NREPS
      parameter(REPS = 10000)
 
      integer	rank, numtasks, tag, avgT, ierr, n 
     
      integer dest, source, status(MPI_STATUS_SIZE)
      double precision T1, T2, sumT, deltaT
      integer, allocatable :: timesx(:), nztimesx(:,:)
      integer, allocatable :: sourceidx(:),sourceidxf(:)
      integer, allocatable :: destidx(:),destidxf(:),timesfx(:)
      real, allocatable :: avgTC(:)
      real, allocatable ::   sumTC(:)
      character msg

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      
c     contiguous communication
      open(unit=2,file="latencytime1.o",status="unknown")
      allocate(avgTC(0:numtasks-1),sumTC(0:numtasks-1))
      allocate(sourceidx(0:numtasks-1),destidx(0:numtasks-1))
      allocate(timesx(0:numtasks-1),timesfx(0:numtasks*numtasks-1))
      allocate(sourceidxf(0:numtasks*numtasks-1))   
      allocate(destidxf(0:numtasks*numtasks-1)) 
      allocate(nztimesx(0:numtasks*numtasks-1,3))
C     CONTIGUOUS COMMUNICATION


      sourceidx=-1.d0; destidx=-1.d0
      timesf=0.d0; sourceidxf=-1.d0; destidxf=-1.d0
      nztimesx=0.d0; timesfx=-1.d0
      
      sumTC = 0.0
      do 11 n = 1, REPS
      iter=1
      do 101 l= 0,numtasks-2
      do 1001 j= 0, numtasks-1
      
      if (iter.gt.numtasks-1) then
        iter=iter-numtasks
        endif
        i=iter+l
        if (i.gt.numtasks-1) then
        i=i-numtasks
      endif    
     
      tag=i
      
         if (rank .eq. j) then
            if (i-1.eq.j) then    
           !  write(*,*) 'master',rank,'has started...'
         endif
c$$$        write(*,*) 'Beginning latency timing test. Number of reps=',REPS                                                                                   
c$$$        write(*,*) '***********************************************'                                                                                 
c$$$        write(*,*)'Rep#       T1                T2            deltaT'                                                                            
                             
            
            dest = i
            source = i
            NREPS = 0
               ! write(*,*) 'source master',source,'destination master',
!     &              dest  
            
               NREPS=NREPS+1
C     Get start time                                                                                                                                         
               T1 = MPI_WTIME()
               call MPI_SENDRECV(msg,1,MPI_CHARACTER,dest,tag,
     &              msg,1,MPI_CHARACTER,source,tag,MPI_COMM_WORLD,
     &              status,ierr)           
 
C     Get ending time                                                                                                                                        
               T2 = MPI_WTIME()
               deltaT = T2 - T1

               if (REPS.eq.n) then
                 sourceidx(i)=j
                 destidx(i)=dest 
               end if

               sumTC(i) =  deltaT+sumTC(i)
            

        
C          Print final average from all round trips                                                                                                  
               
c$$$        write(*,*)'**********************************************'                                                                                
c$$$        write(*,*)'*** send=',dest, 'receive=', source                                                                                                                                     
c$$$        write(*,*)'*** Avg round trip time=', avgT, 'microseconds'                                                                                         
c$$$        write(*,*)'*** Avg one way latency=', avgT/2, 'microseconds'    
         
         endif                                                                             
         
         if (rank .eq. i) then                                                                                                                   
        ! print*,'test',rank           
           !write(*,*)'task',rank,'has started...'                                                                                                         
            dest = j                                                                                                                                         
            source = j                                                                                                                               
            !write(*,*) 'source',source,'destination',dest      
                                                                                                            
               call MPI_SENDRECV(msg,1,MPI_CHARACTER,source,tag,
     &              msg,1,MPI_CHARACTER,dest,tag,MPI_COMM_WORLD,
     &              status,ierr)                      
         endif                                             
                                                                                                                                                
      iter=iter+1
      
1001  continue
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)  
101   continue
11    continue 
      avgTC = (sumTC * 1000000) 
      !timesx=avgTC
       !print*,desti
c      destidx(i)=dest   
      timesx=avgTC/REPS
       print*,timesx 
c     call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      call MPI_ALLGATHER(timesx,numtasks,MPI_INT,
     &     timesfx,numtasks,MPI_INT,MPI_COMM_WORLD,ierr) 
      call MPI_ALLGATHER(sourceidx,numtasks,MPI_INT,
     &     sourceidxf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr)
      call MPI_ALLGATHER(destidx,numtasks,MPI_INT,
     &     destidxf,numtasks,MPI_INT,MPI_COMM_WORLD,ierr)

     
      call MPI_FINALIZE(ierr)

      print*,'lalal'
      print*,timesfx
       aux=1
      do 41 i=0,numtasks*(numtasks)-1
          nztimesx(i,1)=sourceidxf(i)
          nztimesx(i,2)=destidxf(i)
          nztimesx(i,3)=timesfx(i)
41    continue
      do 51 i=0,numtasks*numtasks-1
          if (nztimesx(i,1).ne.nztimesx(i,2)) then
             write(2,*)(nztimesx(i,j),j=1,3)  
          end if   
51    continue

      end

