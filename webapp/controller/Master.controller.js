sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"com/example/requests/util/formatter"
], function (Controller, formatter) {
	"use strict";

	return Controller.extend("com.example.requests.controller.Master", {
		formatter: formatter,

		onInit: function () {},

		onRequestPress: function (oEvent) {
			var oItem = oEvent.getParameter("listItem");
			var oCtx = oItem.getBindingContext();
			var sId = oCtx.getProperty("id");
			this.getOwnerComponent().getRouter().navTo("Detail", { requestId: sId });
		}
	});
});