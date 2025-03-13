#include <stdlib.h>
#include <stdio.h>

#include <rtl/retcode.h>
#include <coresrv/entity/entity_api.h>
#include <services/handle/if_connection.h>

static const EntityInfo taskInfo_Client = {
    .eiid = "Client",
    .max_endpoints = 0,
    .endpoints = NK_NULL,
};

static const EntityInfo taskInfo_Server = {
    .eiid = "Server",
    .max_endpoints = 0,
    .endpoints = NK_NULL,
};

int main(void) {

    const char * taskInfo_Server_args[] = {"Server", RTL_NULL};
    Entity *task1 = EntityInitEx(&taskInfo_Server, "Server", "Server");

    if (!task1) {
        fprintf(stderr, "Can't initialize task \"Server\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task1, taskInfo_Server_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Server\"\n");
        return EXIT_FAILURE;
    }

    if (EntityRun(task1) != rcOk) {
        fprintf(stderr, "Can't run task \"Server\"\n");
        return EXIT_FAILURE;
    }

    /*--------------------------------------------------------*/

    const char * taskInfo_Client_args[] = {"Client", RTL_NULL};
    Entity *task2 = EntityInitEx(&taskInfo_Client, "Client", "Client");

    if (!task2) {
        fprintf(stderr, "Can't initialize task \"Client\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task2, taskInfo_Client_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Client\"\n");
        return EXIT_FAILURE;
    }

    if (EntityRun(task2) != rcOk) {
        fprintf(stderr, "Can't run task \"Client\"\n");
        return EXIT_FAILURE;
    }


    return EXIT_SUCCESS;
}
