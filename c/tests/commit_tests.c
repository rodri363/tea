#include "internal.h"

int testBegin(){

	commit_transaction();
	commit_transaction();
	begin_transaction();
	begin_transaction();
	commit_transaction();
	begin_transaction();
	commit_transaction();
	begin_transaction();
	begin_transaction();
	commit_transaction();
	return 0;
}
