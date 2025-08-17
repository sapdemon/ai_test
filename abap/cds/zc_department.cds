@EndUserText.label: 'C: Department'
@AccessControl.authorizationCheck: #CHECK
define view entity ZC_Department
  as projection on ZI_Department
{
	key DepartmentId,
	Name
}