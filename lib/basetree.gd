# vim: ft=gap sts=2 et sw=2

DeclareGlobalFunction("NEWSS_AddChainToTree");
DeclareGlobalFunction("NEWSS_FindChainWithBasePrefix");
DeclareGlobalFunction("NEWSS_RemoveOldChain");

BindGlobal("NEWSS_DEFAULT_TREE_DEPTH", 5);
BindGlobal("NEWSS_DEFAULT_TREE_BOUND", 20);
