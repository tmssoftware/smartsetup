// I think WEAKLINKRTTI and RTTI directives are "too dangerous" to be put in
// a so general-use include like tmscommon.inc, which is something that is shared between each project.
// Let each project decide if it wants to remove RTTI or not
{ Reduce EXE size by disabling as much of RTTI as possible (delphi 2009/2010) }
{.$WEAKLINKRTTI ON} //this affects the whole project, not just the unit, at link time.
{.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q+,R+,S-,T+,U-,V+,W-,X+,Y+,Z1}
{$SCOPEDENUMS ON}
{$WEAKLINKRTTI ON}
{$ZEROBASEDSTRINGS OFF}
