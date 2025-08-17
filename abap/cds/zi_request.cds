@EndUserText.label: 'I: Request'
@Metadata.allowExtensions: true
define root view entity ZI_Request
  as select from zreq_hdr
{
	key req_uuid     as RequestUUID,
		req_id        as RequestID,
		created_at    as CreatedAt,
		description   as Description,
		composition [0..*] of ZI_RequestItem as _Items
}

@EndUserText.label: 'I: Request Item'
@Metadata.allowExtensions: true
define view entity ZI_RequestItem
  as select from zreq_item
{
	key item_uuid     as ItemUUID,
		parent_uuid   as RequestUUID,
		position_no   as PositionNo,
		amount        as Amount,
		currency      as Currency,
		description   as Description,
		department_id as DepartmentId,
		association to parent ZI_Request as _Request on _Request.RequestUUID = RequestUUID
}

@EndUserText.label: 'I: Department'
define view entity ZI_Department as select from zdepartment {
	key id     as DepartmentId,
		name   as Name
}