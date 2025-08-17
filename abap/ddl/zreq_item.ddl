@EndUserText.label: 'Request Item'
@AbapCatalog.enhancementCategory: #NOT_EXTENSIBLE
@AbapCatalog.tableCategory: #TRANSPARENT
@AbapCatalog.deliveryClass: #A
@AbapCatalog.dataMaintenance: #RESTRICTED
define table zreq_item {
	key client        : abap.clnt    not null;
	key item_uuid     : sysuuid_x16  not null;
	parent_uuid       : sysuuid_x16  not null;
	position_no       : abap.int4;
	amount            : abap.dec(15,2);
	currency          : abap.cuky;
	description       : abap.char(120);
	department_id     : abap.char(10);
}