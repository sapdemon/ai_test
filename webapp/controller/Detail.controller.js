sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/ui/model/json/JSONModel",
	"sap/ui/core/Fragment",
	"sap/m/MessageToast",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"com/example/requests/model/RequestService",
	"com/example/requests/util/formatter"
], function (Controller, JSONModel, Fragment, MessageToast, Filter, FilterOperator, RequestService, formatter) {
	"use strict";

	return Controller.extend("com.example.requests.controller.Detail", {
		formatter: formatter,

		onInit: function () {
			this.getOwnerComponent().getRouter().getRoute("Detail").attachPatternMatched(this._onRouteMatched, this);
		},

		_onRouteMatched: function (oEvent) {
			var sId = oEvent.getParameter("arguments").requestId;
			var oModel = this.getOwnerComponent().getModel();
			var aRequests = oModel.getProperty("/requests") || [];
			var iIdx = aRequests.findIndex(function (r) { return String(r.id) === String(sId); });
			if (iIdx > -1) {
				this.getView().bindElement({ path: "/requests/" + iIdx });
			}
		},

		onAddPosition: function () {
			var oView = this.getView();
			if (!this._pDialog) {
				this._pDialog = Fragment.load({
					name: "com.example.requests.fragment.PositionDialog",
					controller: this
				}).then(function (oDialog) {
					oView.addDependent(oDialog);
					return oDialog;
				});
			}
			this._pDialog.then(function (oDialog) {
				var oNew = RequestService.createEmptyPosition();
				oDialog.setModel(new JSONModel(oNew), "pos");
				oDialog.open();
			}.bind(this));
		},

		onSavePosition: function () {
			var oDialog = this.byId("positionDialog");
			var oPos = oDialog.getModel("pos").getData();
			var iRequestId = this.getView().getBindingContext().getProperty("id");
			RequestService.addPosition(iRequestId, oPos);
			oDialog.close();
			MessageToast.show(this.getResourceBundle().getText("msg.positionAdded"));
		},

		onCancelPosition: function () {
			this.byId("positionDialog").close();
		},

		onDeletePosition: function (oEvent) {
			var oContext = oEvent.getParameter("listItem").getBindingContext();
			var iPosNumber = oContext.getProperty("positionNumber");
			var iRequestId = this.getView().getBindingContext().getProperty("id");
			RequestService.deletePosition(iRequestId, iPosNumber);
			MessageToast.show(this.getResourceBundle().getText("msg.positionDeleted"));
		},

		onDepartmentValueHelp: function (oEvent) {
			var oInput = oEvent.getSource();
			var oView = this.getView();
			if (!this._pDeptVH) {
				this._pDeptVH = Fragment.load({
					name: "com.example.requests.fragment.DepartmentValueHelp",
					controller: this
				}).then(function (oDialog) {
					oView.addDependent(oDialog);
					return oDialog;
				});
			}
			this._pDeptVH.then(function (oDialog) {
				oDialog.data("field", oInput);
				oDialog.open();
			});
		},

		onDepartmentSearch: function (oEvent) {
			var sValue = oEvent.getParameter("value");
			var oFilter = new Filter({
				filters: [
					new Filter("id", FilterOperator.Contains, sValue),
					new Filter("name", FilterOperator.Contains, sValue)
				],
				and: false
			});
			oEvent.getSource().getBinding("items").filter([oFilter]);
		},

		onDepartmentConfirm: function (oEvent) {
			var oSelectedItem = oEvent.getParameter("selectedItem");
			if (!oSelectedItem) { return; }
			var oSelected = oSelectedItem.getBindingContext("departments").getObject();
			var oField = oEvent.getSource().data("field");
			oField.setValue(oSelected.id);
		},

		onDepartmentCancel: function () {
			// no-op
		},

		getResourceBundle: function () {
			return this.getOwnerComponent().getModel("i18n").getResourceBundle();
		}
	});
});