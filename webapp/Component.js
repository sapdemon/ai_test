sap.ui.define([
	"sap/ui/core/UIComponent",
	"sap/ui/model/json/JSONModel",
	"com/example/requests/model/models",
	"com/example/requests/model/RequestService"
], function (UIComponent, JSONModel, models, RequestService) {
	"use strict";

	return UIComponent.extend("com.example.requests.Component", {
		metadata: {
			manifest: "json"
		},

		init: function () {
			UIComponent.prototype.init.apply(this, arguments);

			this.setModel(models.createDeviceModel(), "device");

			var oDataModel = new JSONModel();
			oDataModel.loadData("model/Requests.json");
			this.setModel(oDataModel);

			var oDepartmentsModel = new JSONModel();
			oDepartmentsModel.loadData("model/Departments.json");
			this.setModel(oDepartmentsModel, "departments");

			var fnTryInitService = function () {
				if (!oDataModel.getProperty("/requests") || !oDepartmentsModel.getProperty("/departments")) {
					return;
				}
				RequestService.initialize(oDataModel, oDepartmentsModel);
			};

			oDataModel.attachRequestCompleted(fnTryInitService);
			oDepartmentsModel.attachRequestCompleted(fnTryInitService);

			this.getRouter().initialize();
		}
	});
});