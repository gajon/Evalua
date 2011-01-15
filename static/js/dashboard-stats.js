$(document).ready(function () {
	var tableFound = $(this).find("#id-table-stats").find("tbody").find("tr");
	if (tableFound && tableFound.length != 0) {
		$("#id-table-stats").tablesorter({sortList: [[0,0]]});
	}
});
