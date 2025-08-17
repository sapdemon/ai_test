@EndUserText.label: 'Requests Header'
@AbapCatalog.enhancementCategory: #NOT_EXTENSIBLE
@AbapCatalog.tableCategory: #TRANSPARENT
@AbapCatalog.deliveryClass: #A
@AbapCatalog.dataMaintenance: #RESTRICTED
define table zreq_hdr {
	key client       : abap.clnt    not null;
	key req_uuid     : sysuuid_x16  not null;
	req_id           : abap.int4;
	created_at       : abap.utclong;
	description      : abap.char(120);
}