{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 1.0                                                         }
{                                                                             }
{    Copyright (c) 2003-2007, Sean B. Durkin (s.durkin@gridsense.net)         }
{ The public is hereby granted free and unrestricted license to use and       }
{ to copy this unit.                                                          }
{                                                                             }
{*****************************************************************************}

unit uSBD_Resources;
interface

resourcestring
  sCannotDestroy = 'Cannot destroy TIntfProvider whilst inside a deregistration event handler';
  sCannotDereg   = 'Cannot deregister TIntfProvider whilst inside a deregistration event handler for another item.';
  sCannotReg     = 'Cannot register TIntfProvider whilst inside a deregistration event handler.';
  sOverloadDisplayName = 'Cannot register with an overloaded Display Name';
  sBadName       = 'Name does not follow nameing rules.';

  // The clarity of the following error message needs to be improved.
  // Any suggestions any one?
  sUnbalancedReferenceCount = 'References to object of class %s must be balanced over construction.';

  SInt64ToIntegerConv = 'Overflow error in list conversion (index %d)';

implementation

end.
