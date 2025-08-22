module StateSerialization

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use SnowWaterRenew
  use SnowLayerChange
  use messagepack
  use iso_fortran_env

  implicit none
    
contains

SUBROUTINE forcing_serialization (forcing, mp_arr)
    type(ForcingType), intent(in) :: forcing
    class(mp_arr_type), allocatable, intent(out) :: mp_arr
    mp_arr = mp_arr_type(22)
    mp_arr%values(1)%obj = mp_float_type(forcing%UU) !UU
    mp_arr%values(2)%obj = mp_float_type(forcing%VV) !VV
    mp_arr%values(3)%obj = mp_float_type(forcing%SFCTMP) !SFCTMP
    mp_arr%values(4)%obj = mp_float_type(forcing%Q2) !Q2
    mp_arr%values(5)%obj = mp_float_type(forcing%SFCPRS) !SFCPRS
    mp_arr%values(6)%obj = mp_float_type(forcing%SOLDN) !SOLDN
    mp_arr%values(7)%obj = mp_float_type(forcing%LWDN) !LWDN
    mp_arr%values(8)%obj = mp_int_type(forcing%YEARLEN) !YEARLEN, out
    mp_arr%values(9)%obj = mp_float_type(forcing%JULIAN) !JULIAN, out
    mp_arr%values(10)%obj = mp_float_type(forcing%THAIR) !THAIR, out
    mp_arr%values(11)%obj = mp_float_type(mforcing%QAIR) !QAIR, out
    mp_arr%values(12)%obj = mp_float_type(forcing%EAIR) !EAIR, out
    mp_arr%values(13)%obj = mp_float_type(forcing%RHOAIR) !RHOAIR, out
    mp_arr%values(14)%obj = mp_float_type(forcing%O2PP) !O2PP
    mp_arr%values(15)%obj = mp_float_type(forcing%CO2PP) !CO2PP
    mp_arr%values(16)%obj = mp_float_type(forcing%SWDOWN) !SWDOWN, out
    mp_arr%values(17)%obj = mp_float_type(forcing%SOLAD) !SOLAD
    mp_arr%values(18)%obj = mp_float_type(forcing%SOLAI) !SOLAI
    mp_arr%values(19)%obj = mp_float_type(forcing%PRCP) !PRCP
    mp_arr%values(20)%obj = mp_float_type(forcing%PRCPNONC) !PRCPNONC
    mp_arr%values(21)%obj = mp_float_type(mforcing%FPICE) !FPICE, out 
    mp_arr%values(22)%obj = mp_float_type(forcing%UR) !UR, out

END SUBROUTINE forcing_serialization

SUBROUTINE domain_serialization (domain, mp_arr)
    type(DomainType), intent(in) :: domain
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(6)
    mp_arr%values(1)%obj = mp_float_type(domain%curr_datetime) !curr_datetime
    mp_arr%values(2)%obj = mp_float_type(domain%ITIME) !ITIME
    mp_arr%values(3)%obj = mp_float_type(domain%time_dbl) !time_dbl
    mp_arr%values(4)%obj = mp_float_type(domain%nowdate) !nowdate

    mp_arr%values(5)%obj = mp_arr_type(domain%DZSNSO) !DZSNSO have to fix indices using levels%soil
    mp_arr%values(6)%obj = mp_arr_type(domain%ZSNSO) !ZSNSO have to fix indices using levels%soil

END SUBROUTINE domain_serialization 

SUBROUTINE energy_serialization (energy, mp_arr)
    type(EnergyType), intent(in) :: energy
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(125)
    mp_arr%values(1)%obj = mp_float_type(energy%cosz) !cosz
    mp_arr%values(2)%obj = mp_float_type(energy%cosz_horiz) !cosz_horiz
    mp_arr%values(3)%obj = mp_float_type(energy%TAH) !TAH
    mp_arr%values(4)%obj = mp_float_type(energy%EAH) !EAH
    mp_arr%values(5)%obj = mp_float_type(energy%IGS) !IGS
    mp_arr%values(6)%obj = mp_float_type(energy%TAUXV) !TAUXV
    mp_arr%values(7)%obj = mp_float_type(energy%TAUYV) !TAUYV
    mp_arr%values(8)%obj = mp_float_type(energy%IRC) !IRC
    mp_arr%values(9)%obj = mp_float_type(energy%SHC) !SHC
    mp_arr%values(10)%obj = mp_float_type(energy%IRG) !IRG
    mp_arr%values(11)%obj = mp_float_type(energy%SHG) !SHG
    mp_arr%values(12)%obj = mp_float_type(energy%EVG) !EVG
    mp_arr%values(13)%obj = mp_float_type(energy%EVC) !EVC
    mp_arr%values(14)%obj = mp_float_type(energy%TR) !TR
    mp_arr%values(15)%obj = mp_float_type(energy%GHV) !GHV
    mp_arr%values(16)%obj = mp_float_type(energy%PSNSUN) !PSNSUN
    mp_arr%values(17)%obj = mp_float_type(energy%PSNSHA) !PSNSHA
    mp_arr%values(18)%obj = mp_float_type(energy%T2MV) !T2MV
    mp_arr%values(19)%obj = mp_float_type(energy%Q2V) !Q2V
    mp_arr%values(20)%obj = mp_float_type(energy%CHV) !CHV
    mp_arr%values(21)%obj = mp_float_type(energy%CHLEAF) !CHLEAF
    mp_arr%values(22)%obj = mp_float_type(energy%CHUC) !CHUC
    mp_arr%values(23)%obj = mp_float_type(energy%CHV2) !CHV2
    mp_arr%values(24)%obj = mp_float_type(energy%RB) !RB
    mp_arr%values(25)%obj = mp_float_type(energy%Z0MG) !Z0MG
    mp_arr%values(26)%obj = mp_float_type(energy%Z0M) !Z0M
    mp_arr%values(27)%obj = mp_float_type(energy%ZPD) !ZPD
    mp_arr%values(28)%obj = mp_float_type(energy%ZLVL) !ZLVL
    mp_arr%values(29)%obj = mp_float_type(energy%EMG) !EMG
    mp_arr%values(30)%obj = mp_float_type(energy%RSURF) !RSURF
    mp_arr%values(31)%obj = mp_float_type(energy%RHSUR) !RHSUR
    mp_arr%values(32)%obj = mp_float_type(energy%LATHEAV) !LATHEAV
    mp_arr%values(33)%obj = mp_float_type(energy%frozen_canopy) !frozen_canopy
    mp_arr%values(34)%obj = mp_float_type(energy%GAMMAV) !GAMMAV
    mp_arr%values(35)%obj = mp_float_type(energy%LATHEAG) !LATHEAG
    mp_arr%values(36)%obj = mp_float_type(energy%frozen_ground) !frozen_ground
    mp_arr%values(37)%obj = mp_float_type(energy%GAMMAG) !GAMMAG
    mp_arr%values(38)%obj = mp_float_type(energy%TGB) !TGB
    mp_arr%values(39)%obj = mp_float_type(energy%CMB) !CMB
    mp_arr%values(40)%obj = mp_float_type(energy%CHB) !CHB
    mp_arr%values(41)%obj = mp_float_type(energy%Z0WRF) !Z0WRF
    mp_arr%values(42)%obj = mp_float_type(energy%RSSUN) !RSSUN
    mp_arr%values(43)%obj = mp_float_type(energy%T2M) !T2M
    mp_arr%values(44)%obj = mp_float_type(energy%Q1) !Q1
    mp_arr%values(45)%obj = mp_float_type(energy%Q2E) !Q2E
    mp_arr%values(46)%obj = mp_float_type(energy%FGEV) !FGEV
    mp_arr%values(47)%obj = mp_float_type(energy%TS) !TS
    mp_arr%values(48)%obj = mp_float_type(energy%TAUY) !TAUY
    mp_arr%values(49)%obj = mp_float_type(energy%GH) !GH
    mp_arr%values(50)%obj = mp_float_type(energy%SSOIL) !SSOIL
    mp_arr%values(51)%obj = mp_float_type(energy%TGV) !TGV
    mp_arr%values(52)%obj = mp_float_type(energy%FCEV) !FCEV
    mp_arr%values(53)%obj = mp_float_type(energy%CM) !CM
    mp_arr%values(54)%obj = mp_float_type(energy%FIRA) !FIRA
    mp_arr%values(55)%obj = mp_float_type(energy%RSSHA) !RSSHA
    mp_arr%values(56)%obj = mp_float_type(energy%TG) !TG
    mp_arr%values(57)%obj = mp_float_type(energy%CH) !CH
    mp_arr%values(58)%obj = mp_float_type(energy%FCTR) !FCTR
    mp_arr%values(59)%obj = mp_float_type(energy%PAH) !PAH
    mp_arr%values(60)%obj = mp_float_type(energy%TAUX) !TAUX
    mp_arr%values(61)%obj = mp_float_type(energy%FSH) !FSH
    mp_arr%values(62)%obj = mp_float_type(energy%EMISSI) !EMISSI
    mp_arr%values(63)%obj = mp_float_type(energy%TRAD) !TRAD
    mp_arr%values(64)%obj = mp_float_type(energy%APAR) !APAR
    mp_arr%values(65)%obj = mp_float_type(energy%PSN) !PSN
    mp_arr%values(66)%obj = mp_float_type(energy%STC) !STC
    mp_arr%values(67)%obj = mp_float_type(energy%LH) !LH
    mp_arr%values(68)%obj = mp_float_type(energy%TGS) !TGS
    mp_arr%values(69)%obj = mp_arr_type(energy%HCPCT ) !HCPCT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(70)%obj = mp_arr_type(energy%DF ) !DF array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(71)%obj = mp_arr_type(energy%FACT ) !FACT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(72)%obj = mp_float_type(energy%PAHV) !PAHV
    mp_arr%values(73)%obj = mp_float_type(energy%PAHG) !PAHG
    mp_arr%values(74)%obj = mp_float_type(energy%PAHB) !PAHB
    mp_arr%values(75)%obj = mp_float_type(energy%FSHA) !FSHA
    mp_arr%values(76)%obj = mp_float_type(energy%LAISUN) !LAISUN
    mp_arr%values(77)%obj = mp_float_type(energy%LAISHA) !LAISHA
    mp_arr%values(78)%obj = mp_float_type(energy%BGAP) !BGAP
    mp_arr%values(79)%obj = mp_float_type(energy%WGAP) !WGAP
    mp_arr%values(80)%obj = mp_arr_type(energy%ALBD)  !ALBD array  (1:parameters%NBAND)
    mp_arr%values(81)%obj = mp_arr_type(energy%ALBI) !ALBI array  (1:parameters%NBAND)
    mp_arr%values(82)%obj = mp_arr_type(energy%ALBGRD) !ALBGRD array  (1:parameters%NBAND)
    mp_arr%values(83)%obj = mp_arr_type(energy%ALBGRI) !ALBGRI array  (1:parameters%NBAND)
    mp_arr%values(84)%obj = mp_arr_type(energy%ALBSND) !ALBSND array  (1:parameters%NBAND)
    mp_arr%values(85)%obj = mp_arr_type(energy%ALBSNI) !ALBSNI array  (1:parameters%NBAND)
    mp_arr%values(86)%obj = mp_arr_type(energy%FABD) !FABD array  (1:parameters%NBAND)
    mp_arr%values(87)%obj = mp_arr_type(energy%FABI) !FABI array  (1:parameters%NBAND)
    mp_arr%values(88)%obj = mp_arr_type(energy%FTDD) !FTDD array  (1:parameters%NBAND)
    mp_arr%values(89)%obj = mp_arr_type(energy%FTID) !FTID array  (1:parameters%NBAND)
    mp_arr%values(90)%obj = mp_arr_type(energy%FTII) !FTII array  (1:parameters%NBAND)
    mp_arr%values(91)%obj = mp_arr_type(energy%RHO) !RHO array  (1:parameters%NBAND)
    mp_arr%values(92)%obj = mp_arr_type(energy%TAU) !TAU array  (1:parameters%NBAND)
    mp_arr%values(93)%obj = mp_float_type(energy%FSUN) !FSUN
    mp_arr%values(94)%obj = mp_float_type(energy%TAUSS) !TAUSS
    mp_arr%values(95)%obj = mp_float_type(energy%FAGE) !FAGE
    mp_arr%values(96)%obj = mp_float_type(energy%ALB) !ALB
    mp_arr%values(97)%obj = mp_float_type(energy%ALBOLD) !ALBOLD
    mp_arr%values(98)%obj = mp_arr_type(energy%FTDI) !FTDI array  (1:2)
    mp_arr%values(99)%obj = mp_arr_type(energy%FREVD) !FREVD array  (1:2)
    mp_arr%values(100)%obj = mp_arr_type(energy%FREGD) !FREGD array  (1:2)
    mp_arr%values(101)%obj = mp_arr_type(energy%FREVI) !FREVI array  (1:2)
    mp_arr%values(102)%obj = mp_arr_type(energy%FREGI) !FREGI array  (1:2)
    mp_arr%values(103)%obj = mp_float_type(energy%SAG) !SAG
    mp_arr%values(104)%obj = mp_float_type(energy%SAV) !SAV
    mp_arr%values(105)%obj = mp_float_type(energy%FSA) !FSA
    mp_arr%values(106)%obj = mp_float_type(energy%PARSUN) !PARSUN
    mp_arr%values(107)%obj = mp_float_type(energy%PARSHA) !PARSHA
    mp_arr%values(108)%obj = mp_float_type(energy%FSR) !FSR
    mp_arr%values(109)%obj = mp_float_type(energy%FSRV) !FSRV
    mp_arr%values(110)%obj = mp_float_type(energy%FSRG) !FSRG
    mp_arr%values(111)%obj = mp_float_type(energy%QSFC) !QSFC
    mp_arr%values(112)%obj = mp_float_type(energy%TV) !TV
    mp_arr%values(113)%obj = mp_float_type(energy%CAH2) !CAH2
    mp_arr%values(114)%obj = mp_float_type(energy%IRB) !IRB
    mp_arr%values(115)%obj = mp_float_type(energy%SHB) !SHB
    mp_arr%values(116)%obj = mp_float_type(energy%EVB) !EVB
    mp_arr%values(117)%obj = mp_float_type(energy%GHB) !GHB
    mp_arr%values(118)%obj = mp_float_type(energy%TAUXB) !TAUXB
    mp_arr%values(119)%obj = mp_float_type(energy%TAUYB) !TAUYB
    mp_arr%values(120)%obj = mp_float_type(energy%EHB2) !EHB2
    mp_arr%values(121)%obj = mp_float_type(energy%T2MB) !T2MB
    mp_arr%values(122)%obj = mp_float_type(energy%Q2B) !Q2B
    mp_arr%values(123)%obj = mp_arr_type(energy%IMELT) !IMELT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(124)%obj = mp_float_type(energy%QMELT) !QMELT
    mp_arr%values(125)%obj = mp_arr_type(energy%SNOWT_AVG) !SNOWT_AVG array,  could be realMissing

END SUBROUTINE energy_serialization

SUBROUTINE water_serialization (water, mp_arr)
    type(WaterType), intent(in) :: water
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(67)
    mp_arr%values(1)%obj = mp_float_type(water%FP) !FP
    mp_arr%values(2)%obj = mp_float_type(water%RAIN) !RAIN
    mp_arr%values(3)%obj = mp_float_type(water%SNOW) !SNOW
    mp_arr%values(4)%obj = mp_float_type(water%BDFALL) !BDFALL
    mp_arr%values(5)%obj = mp_float_type(water%QINTR) !QINTR
    mp_arr%values(6)%obj = mp_float_type(water%QDRIPR) !QDRIPR
    mp_arr%values(7)%obj = mp_float_type(water%QTHROR) !QTHROR
    mp_arr%values(8)%obj = mp_float_type(water%QINTS) !QINTS
    mp_arr%values(9)%obj = mp_float_type(water%QDRIPS) !QDRIPS
    mp_arr%values(10)%obj = mp_float_type(water%QTHROS) !QTHROS
    mp_arr%values(11)%obj = mp_float_type(water%QRAIN) !QRAIN
    mp_arr%values(12)%obj = mp_float_type(water%QSNOW) !QSNOW
    mp_arr%values(13)%obj = mp_float_type(water%SNOWHIN) !SNOWHIN
    mp_arr%values(14)%obj = mp_float_type(water%CANLIQ) !CANLIQ
    mp_arr%values(15)%obj = mp_float_type(water%CANICE) !CANICE
    mp_arr%values(16)%obj = mp_float_type(water%FWET) !FWET
    mp_arr%values(17)%obj = mp_float_type(water%CMC) !CMC
    mp_arr%values(18)%obj = mp_float_type(water%FSNO) !FSNO
    mp_arr%values(19)%obj = mp_float_type(water%BDSNO) !BDSNO
    mp_arr%values(20)%obj = mp_float_type(water%BTRAN) !BTRAN
    mp_arr%values(21)%obj = mp_array_type(water%BTRANI ) !BTRANI array (1:levels%NSOIL)
    mp_arr%values(22)%obj = mp_array_type(water%SNICEV ) !SNICEV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(23)%obj = mp_array_type(water%EPORE ) !EPORE array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(24)%obj = mp_array_type(water%SNLIQV ) !SNLIQV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(25)%obj = mp_array_type(water%DF ) !DF  array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(26)%obj = mp_array_type(water%HCPCT ) !HCPCT array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(27)%obj = mp_array_type(water%SICE ) !SICE array (1:levels%NSOIL)
    mp_arr%values(28)%obj = mp_float_type(water%SNEQV) !SNEQV
    mp_arr%values(29)%obj = mp_float_type(water%SNOWH) !SNOWH
    mp_arr%values(30)%obj = mp_array_type(water%SH2O ) !SH2O array (1:levels%NSOIL)
    mp_arr%values(31)%obj = mp_array_type(water%SMC ) !SMC array (1:levels%NSOIL)
    mp_arr%values(32)%obj = mp_array_type(water%SNICE ) !SNICE array (-levels%NSNOW+1:0)
    mp_arr%values(33)%obj = mp_array_type(water%SNLIQ ) !SNLIQ array (-levels%NSNOW+1:0)
    mp_arr%values(34)%obj = mp_float_type(water%PONDING) !PONDING
    mp_arr%values(35)%obj = mp_float_type(water%SNEQVO) !SNEQVO
    mp_arr%values(36)%obj = mp_float_type(water%QVAP) !QVAP
    mp_arr%values(37)%obj = mp_float_type(water%QDEW) !QDEW
    mp_arr%values(38)%obj = mp_float_type(water%QSNSUB) !QSNSUB
    mp_arr%values(39)%obj = mp_float_type(water%QSEVA) !QSEVA
    mp_arr%values(40)%obj = mp_float_type(water%QSNFRO) !QSNFRO
    mp_arr%values(41)%obj = mp_float_type(water%QSDEW) !QSDEW
    mp_arr%values(42)%obj = mp_float_type(water%QINSUR) !QINSUR
    mp_arr%values(43)%obj = mp_float_type(water%ACSNOM) !ACSNOM
    mp_arr%values(44)%obj = mp_array_type(water%ETRANI ) !ETRANI array (1:levels%NSOIL)
    mp_arr%values(45)%obj = mp_float_type(water%RUNSRF) !RUNSRF
    mp_arr%values(46)%obj = mp_float_type(water%WSLAKE) !WSLAKE
    mp_arr%values(47)%obj = mp_float_type(water%EVAPOTRANS) !EVAPOTRANS
    mp_arr%values(48)%obj = mp_float_type(water%ECAN) !ECAN
    mp_arr%values(49)%obj = mp_float_type(water%ETRAN) !ETRAN
    mp_arr%values(50)%obj = mp_float_type(water%SNOFLOW) !SNOFLOW
    mp_arr%values(51)%obj = mp_float_type(water%PONDING1) !PONDING1
    mp_arr%values(52)%obj = mp_float_type(water%PONDING2) !PONDING2
    mp_arr%values(53)%obj = mp_integer_type(water%ISNOW) !ISNOW integer
    mp_arr%values(54)%obj = mp_array_type(water%DZSNSO ) !DZSNSO array (-levels%nsnow+1:levels%nsoil)
    mp_arr%values(55)%obj = mp_float_type(water%QSNBOT) !QSNBOT
    mp_arr%values(56)%obj = mp_float_type(water%RUNSUB) !RUNSUB
    mp_arr%values(57)%obj = mp_float_type(water%PDDUM) !PDDUM
    mp_arr%values(58)%obj = mp_float_type(water%runsrf_dt) !runsrf_dt
    mp_arr%values(59)%obj = mp_array_type(water%FCR ) !FCR array (1:levels%nsoil)
    mp_arr%values(60)%obj = mp_float_type(water%SICEMAX) !SICEMAX
    mp_arr%values(61)%obj = mp_float_type(water%FCRMAX) !FCRMAX
    mp_arr%values(62)%obj = mp_float_type(water%FACC) !FACC
    mp_arr%values(63)%obj = mp_float_type(water%QDRAIN) !QDRAIN
    mp_arr%values(64)%obj = mp_float_type(water%DEEPRECH) !DEEPRECH
    mp_arr%values(65)%obj = mp_float_type(water%ZWT) !ZWT
    mp_arr%values(66)%obj = mp_float_type(water%ASAT) !ASAT
    mp_arr%values(67)%obj = mp_float_type(water%SMCWTD) !SMCWTD


END SUBROUTINE water_serialization

SUBROUTINE parameters_serialization (parameters, mp_arr)
    type(ParametersType), intent(in) :: parameters
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(5)
    mp_arr%values(1)%obj = mp_float_type(parameters%SAI) !SAI
    mp_arr%values(2)%obj = mp_float_type(parameters%LAI) !LAI
    mp_arr%values(3)%obj = mp_float_type(parameters%ESAI) !ESAI
    mp_arr%values(4)%obj = mp_float_type(parameters%ELAI) !ELAI
    mp_arr%values(4)%obj = mp_float_type(parameters%FVEG) !FVEG

END SUBROUTINE parameters_serialization


END Module
