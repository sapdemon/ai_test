sap.ui.define([], function () {
	"use strict";

	var _oModel = null;
	var _oDepartmentsModel = null;

	function _getRequestIndexById(requestId) {
		var aRequests = _oModel.getProperty("/requests") || [];
		for (var i = 0; i < aRequests.length; i++) {
			if (String(aRequests[i].id) === String(requestId)) {
				return i;
			}
		}
		return -1;
	}

	function _getNextPositionNumber(aPositions) {
		if (!aPositions || aPositions.length === 0) {
			return 1;
		}
		var iMax = 0;
		for (var i = 0; i < aPositions.length; i++) {
			if (Number(aPositions[i].positionNumber) > iMax) {
				iMax = Number(aPositions[i].positionNumber);
			}
		}
		return iMax + 1;
	}

	return {
		initialize: function (oDataModel, oDepartmentsModel) {
			_oModel = oDataModel;
			_oDepartmentsModel = oDepartmentsModel;
		},

		createEmptyPosition: function () {
			return {
				positionNumber: null,
				amount: null,
				currency: "UAH",
				description: "",
				departmentId: ""
			};
		},

		addPosition: function (requestId, oPosition) {
			if (!_oModel) { throw new Error("RequestService not initialized"); }
			var iIdx = _getRequestIndexById(requestId);
			if (iIdx < 0) { throw new Error("Request not found: " + requestId); }
			var sBasePath = "/requests/" + iIdx + "/positions";
			var aPositions = _oModel.getProperty(sBasePath) || [];
			oPosition = Object.assign({}, oPosition);
			oPosition.positionNumber = _getNextPositionNumber(aPositions);
			aPositions.push(oPosition);
			_oModel.setProperty(sBasePath, aPositions);
			_oModel.refresh(true);
			return oPosition;
		},

		deletePosition: function (requestId, positionNumber) {
			if (!_oModel) { throw new Error("RequestService not initialized"); }
			var iIdx = _getRequestIndexById(requestId);
			if (iIdx < 0) { throw new Error("Request not found: " + requestId); }
			var sBasePath = "/requests/" + iIdx + "/positions";
			var aPositions = _oModel.getProperty(sBasePath) || [];
			var aFiltered = aPositions.filter(function (p) { return Number(p.positionNumber) !== Number(positionNumber); });
			_oModel.setProperty(sBasePath, aFiltered);
			_oModel.refresh(true);
		}
	};
});