@EndUserText.label: 'Department Directory'
@AbapCatalog.enhancementCategory: #NOT_EXTENSIBLE
@AbapCatalog.tableCategory: #TRANSPARENT
@AbapCatalog.deliveryClass: #A
@AbapCatalog.dataMaintenance: #RESTRICTED
define table zdepartment {
	key client : abap.clnt   not null;
	key id     : abap.char(10);
	name       : abap.char(60);
}