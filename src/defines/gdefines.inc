{$IFDEF FPC}

  {$MODE delphi} {$H+}
  {$modeswitch nestedprocvars}
  {$modeswitch advancedrecords}
  {$define HAS_FORMATSETTINGS}
  {$Define HelperPrimitives}
  {$DEFINE fpc270Greater}

{$else}

  {$IF CompilerVersion > 18.5}
    {$define DelphiNew}
    {$define HAS_FORMATSETTINGS}
  {$else}
    {$define DelphiOld}
  {$IFEND}

{$ENDIF}

{$IFDEF gDefines_inc} error  duplicate gDefines {$ENDIF}
{$define gDefines_inc}

