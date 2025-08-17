@EndUserText.label: 'C: Request'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_Request
  as projection on ZI_Request
{
	key RequestUUID,
	RequestID,
	CreatedAt,
	Description,
	to_Items
}

@EndUserText.label: 'C: Request Item'
@AccessControl.authorizationCheck: #CHECK
define view entity ZC_RequestItem
  as projection on ZI_RequestItem
{
	key ItemUUID,
	RequestUUID,
	PositionNo,
	Amount,
	Currency,
	Description,
	DepartmentId
}

extend view entity ZC_Request with {
	association to ZC_RequestItem as to_Items on to_Items.RequestUUID = $projection.RequestUUID;
}