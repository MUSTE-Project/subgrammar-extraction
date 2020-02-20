
abstract DyckAbs = {

cat S; Dyck; Open; Close; 

fun

  main : Dyck -> S ;

  empty : Dyck ;
  wrap  : Open -> Dyck -> Close -> Dyck ;
  conc  : Dyck -> Dyck -> Dyck ;

  opena : Open ;
  openb : Open ;

  closea : Close ;
  closeb : Close ;

}
