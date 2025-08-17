@EndUserText.label: 'I: Request'
define root view entity ZI_Request
  as select from zreq_hdr
{
	key req_uuid        as RequestUUID,
		client,
		req_id           as RequestID,
		created_at       as CreatedAt,
		description      as Description,
		_COMPOSITION_CHILD( to_Items, ZI_RequestItem )
}

@EndUserText.label: 'I: Request Item'
define view entity ZI_RequestItem
  as select from zreq_item
{
	key item_uuid       as ItemUUID,
		client,
		parent_uuid      as RequestUUID,
		position_no      as PositionNo,
		amount           as Amount,
		currency         as Currency,
		description      as Description,
		department_id    as DepartmentId
}

@EndUserText.label: 'I: Department'
define view entity ZI_Department as select from zdepartment {
	key id     as DepartmentId,
		name   as Name
}

association [0..*] to ZI_RequestItem as _Items on _Items.RequestUUID = $projection.RequestUUID;

extend view entity ZI_Request with {
	association to ZI_RequestItem as to_Items on to_Items.RequestUUID = $projection.RequestUUID;
}