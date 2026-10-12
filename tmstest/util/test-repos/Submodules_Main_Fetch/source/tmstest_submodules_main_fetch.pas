unit tmstest_submodules_main_fetch;

interface
function MainParent: string;

implementation
function MainParent;
begin
 Result := 'MainParent fetch';
end;

end.

