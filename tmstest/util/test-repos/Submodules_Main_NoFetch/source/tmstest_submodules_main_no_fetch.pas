unit tmstest_submodules_main_no_fetch;

interface
function MainParent: string;

implementation
function MainParent;
begin
 Result := 'MainParent no fetch';
end;

end.

