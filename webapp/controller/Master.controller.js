sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"com/example/requests/util/formatter"
], function (Controller, formatter) {
	"use strict";

	return Controller.extend("com.example.requests.controller.Master", {
		formatter: formatter,

		onInit: function () {},

		onRequestPress: function (oEvent) {
			var oCtx = oEvent.getParameter("listItem").getBindingContext();
			var sPath = oCtx.getPath(); // e.g. /Requests(guid'...')
			this.getOwnerComponent().getRouter().navTo("Detail", { path: encodeURIComponent(sPath.slice(1)) });
		}
	});
});