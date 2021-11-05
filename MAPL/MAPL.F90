! This module re-exports the public entities
! of the underlying packages.
module MAPL
   use MAPLBase_mod
   use MAPL_GenericMod
   use MAPL_VarSpecMiscMod
   use MAPL_ExtDataGridCompNG, only: T_EXTDATANG_STATE, EXTDATANG_WRAP
   use ESMF_CFIOMod
   use pFIO
   use MAPL_GridCompsMod
   implicit none
end module MAPL

module MAPL_Mod
   use MAPL
end module MAPL_Mod
   
