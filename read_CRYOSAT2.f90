!!!!!! use ifort.exe
      parameter(n=9999)
!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT
      parameter(icol=51)  ! column 개수
!!!!!!!!!!!!!!!!!!!!!!!!!!
      character*12 a, filen*15 ! col(icol)
      double precision fcol(icol)
      double precision equ_time                     !1985-01-01(jday 31048)부터 sec
      double precision equ_time2jday                ! equ_time/60./60./24.+31048.  !1985년 sec-> day엑셀줄리안데이
      double precision lat1, lon1, lat2, lon2
      real dist, bear
      integer iyear, month, iday, iyearday
      character time*8, date*11, ors*50
      !equ_time = 569126365.487841  !(14-Jan-2003 02:39:25)
      !equ_time2jday = equ_time/60./60./24.+31048.  !1985년 sec-> day엑셀줄리안데이
      !call sub_jul2day(equ_time2jday, date, time, iday, month, iyear, iyearday)
      !print*, equ_time2jday,date,time
      !stop
!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT
      iors= 4            ! 1(이어도) 2(가거초) 3(소청초) 4(제주남부부이)
      dist_r=30.         ! 기지부터반경 km 영역전체100000.km
!!!!!!!!!!!!!!!!!!!!!!!!!!
      idist_r=nint(dist_r)
      
      if(iors.eq.1) then
                 !이어도 1
            write(ors,'(a,i4.4,a)')trim('iors_'),idist_r,trim('km')
            !ors='iors'
            lat1=32.12333333 
            lon1=125.1816667
      elseif(iors.eq.2) then
                 !가거초 2
            write(ors,'(a,i4.4,a)')trim('gors_'),idist_r,trim('km')
            !ors='gors'
            lat1=33.938333333
            lon1=124.59333333
      elseif(iors.eq.3) then
                 !소청초 3
            write(ors,'(a,i4.4,a)')trim('sors_'),idist_r,trim('km')
            !ors='sors'
            lat1=37.4231333333
            lon1=124.7380388889
      elseif(iors.eq.4) then
                 !제주남부부이 4
            write(ors,'(a,i4.4,a)')trim('jjso_'),idist_r,trim('km')
            !ors='jjso'
            lat1=32.0903
            lon1=126.9658
      else
            stop
      endif
      if(dist_r.gt.99999.) ors='all'
!      lat2=32.0868034
!      lon2=124.911736
!      call dist_bear_latlon(lat1, lon1, lat2, lon2, dist, bear)
!      print*, lat1, lon1, lat2, lon2, dist, bear
!      stop
      call system('dir "./radsdata_CRYOSAT2 phase a"\*.asc /b > ascfilelist.lst')
      open(3,file='ascfilelist.lst', status='old')       !dir *.asc /b
      
      do k=1,999999999
      read(3, *, end=888) filen      !ex> e2p0019c085.asc
      !print*, filen
      open(1, file=filen, status='old')
      read(1, *) a, a                       !# RADS_ASC   
      read(1, *) a,a,a                      !# Satellite =
      read(1, *) a,a,a                      !# Phase     =
      read(1, *) a,a,a, icycle             !# Cycle     =
      read(1, *) a,a,a, ipass               !# Pass      =
      read(1, *) a,a,a, equ_time            !# Equ_time  =
      read(1, *) a,a,a, equ_lon             !# Equ_lon   =
      read(1, *) a,a,a                      !# Original  =
      !print*,filen, icycle, ipass, equ_time, equ_lon

!!!!!!!!!!!!!!!!!!!!!!!!!!I수정파일명
      open(2,file='outasc_CRYOSAT2_a_'//trim(ors)//'.csv',recl=1500)
      if(k.eq.1) write(2,21)
      do i=1,icol
      read(1, *)
      enddo
      do i=1,n
      read(1,*,end=999) fcol
      !print*,fcol
      !where(isnan(fcol)) fcol=9999.
      fcol(1)=0
      equ_time=807230330.265522
      equ_time2jday = (fcol(1)+equ_time)/60./60./24.+31048.  !1985년 sec-> day엑셀줄리안데이
      call sub_jul2day(equ_time2jday, date, time, iday, month, iyear, iyearday)
      call dist_bear_latlon(lat1, lon1, fcol(2), fcol(3), dist, bear)
      if(dist.le.dist_r) then
      call sub_jul2day(equ_time2jday, date, time, iday, month, iyear, iyearday)
      !write(2,*) date, time,',', equ_time2jday,',', ((fcol(j),','),j=2,icol), dist,',', bear
      write(2,*) date,',', time,',', equ_time2jday,',', ((fcol(j),','),j=2,icol), dist,',', bear
      !write(2,20) date, time, equ_time2jday, ((fcol(j)),j=2,icol), dist, bear
      endif
      enddo
999   continue
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!수정포맷
21    format('date,jdate,lat,lon,sealevel,Ku_Hs,WW3_Hs,Ku_backscatter,Ku_gaincontrol,windspeed,distance,bearing')
20    format(a11,1x,a8,1h,,f17.10,1h,,2(f10.6,1h,),7(f9.3,1h,),f7.1)
888   stop
      end
      
      
      subroutine dist_bear_latlon(lat1, lon1, lat2, lon2, dist, bear)
      !dist : distance 거리 km
      !bear : bearing 방향 동쪽으로부터 좌표1->좌표2 시계반대방향 deg.
      double precision lat1, lon1, lat2, lon2
      real dist, bear
      r=6378.137 !지구반경 Earth radius in meter
      PI=acos(-1.)
      dist = ACOS(SIN(lat1*PI/180.)*SIN(lat2*PI/180.) + COS(lat1*PI/180.)*COS(lat2*PI/180.)*COS(lon2*PI/180.-lon1*PI/180.))*r
      bear=ATAN2(COS(lat1*PI/180.)*SIN(lat2*PI/180.)-SIN(lat1*PI/180.)*COS(lat2*PI/180.)*COS(lon2*PI/180.-lon1*PI/180.), SIN(lon2*PI/180.-lon1*PI/180.)*COS(lat2*PI/180.))*180/PI
      end subroutine dist_bear_latlon


!      real jday
!      integer iyear, imonth, iday, iyearday
!      character time*8, date*10
!      call sub_jul2day(jday, date, time, day, month, year, yearday)
       subroutine sub_jul2day (jday,dchar,tchar,iday,month,iyear,iyday)
!
!=======================================================================
!                                                                    ===
!  This routine takes the modified Julian date and converts it       ===
!  to a date and time string.                                        ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     JDAY     modified Julian day (integer)                         ===
!                                                                    ===
!  On Output:                                                        ===
!                                                                    ===
!     DCHAR    date string (character)                               ===
!     IDAY     day of the month (integer)                            ===
!     IYDAY    year day (integer)                                    ===
!     IYEAR    year (integer)                                        ===
!     MONTH    month of the year (integer)                           ===
!     TCHAR    time string (character)                               ===
!                                                                    ===
!  Calls:  GREGORIAN                                                 ===
!                                                                    ===
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
        integer iday,ihour,imin,isec,iyday,iyear,julian,month,offset
        real fjulian,hour,min!,jday
        double precision jday
	      character*(*) dchar,tchar
        character*3   mchar(12)
        data mchar /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
!                    original from HOPS model
        data offset /2440000/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Add offset to get true Julian date.
       jday=jday-24981.            !24981은 엑셀시간함수와 맞추기 위해서 추가 1950년 이후는 유효
       julian=offset+int(jday)
!
!      fjulian=jday-int(jday)+0.5
       fjulian=abs(jday-int(jday))
!
       if (fjulian.ge.1.0) then
         julian=julian+1
         fjulian=fjulian-1.0
       endif
       jday=jday+24981.            !24981은 엑셀시간함수와 맞추기 위해서 추가 1950년 이후는 유효
!
!-----------------------------------------------------------------------
!  Compute Gregorian date.
!-----------------------------------------------------------------------
!
       call gregorian (julian,iday,month,iyear,iyday)
!
!-----------------------------------------------------------------------
!  Form date and time strings.
!-----------------------------------------------------------------------
!
       hour=fjulian*24.
       ihour=int(hour)
       min=(hour-float(ihour))*60.
!       min=(hour-float(ihour))*100.
       imin=int(min)
       isec=(min-imin)*60.
!
!       write(dchar,10) mchar(month),iday,iyear
! 10    format(a3,i3,i5)
       write(dchar,10) iyear,month,iday
 10    format(i4,2(1h-,i2.2))
!       print*,1,dchar,ihour, imin, isec
       imin=nint(float(imin)/10.)*10
       isec=0
!       print*,2,dchar,ihour, imin, isec
       if(imin.eq.60)then
       ihour=ihour+1
       imin=0
       endif
       if(ihour.gt.24) print*,'24시 초과있음'
       write(tchar,20) ihour, imin, isec
 20    format(i2.2,':',i2.2,':',i2.2)
       return
       end

       subroutine gregorian (julian,iday,month,iyear,iyday)
!
!=======================================================================
!                                                                    ===
!  This routine converts Julian day number to calendar (Gregorian)   ===
!  date.                                                             ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     JULIAN   Julian day (integer)                                  ===
!                                                                    ===
!  On Ouput:                                                         ===
!                                                                    ===
!     IDAY     day of the month (integer)                            ===
!     IYDAY    year day (integer)                                    ===
!     IYEAR    year (integer)                                        ===
!     MONTH    month of the year (integer)                           ===
!                                                                    ===
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
       integer iday,igreg,ileap,iyd(13),iyday,iydl(13),iyear,ja,jalpha,jb,jc,jd,je,julian,month
       data iyd  /1,32,60,91,121,152,182,213,244,274,305,335,366/
       data iydl /1,32,61,92,122,153,183,214,245,275,306,336,367/
       parameter (igreg=2299161)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
       if (julian.ge.igreg) then
         jalpha=int((float(julian-1867216)-0.25)/36524.25)
         ja=julian+1+jalpha-int(0.25*float(jalpha))
       else
         ja=julian
       endif
       jb=ja+1524
       jc=int(6680.+(float(jb-2439870)-122.1)/365.25)
       jd=365*jc+int(0.25*float(jc))
       je=int(float(jb-jd)/30.6001)
       iday=jb-jd-int(30.6001*float(je))
       month=je-1
       if (month.gt.12) month=month-12
       iyear=jc-4715
       if (month.gt.2) iyear=iyear-1
       if (iyear.le.0)iyear=iyear-1
       ileap=mod(iyear,4)
       if (ileap.eq.0) then
           iyday=iydl(month)+iday-1
       else
           iyday=iyd(month)+iday-1
       endif
       return
       end
