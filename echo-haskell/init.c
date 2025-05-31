#include <stdlib.h>
#include <stdio.h>

#include <rtl/retcode.h>
#include <coresrv/entity/entity_api.h>
#include <services/handle/if_connection.h>

static const EntityInfo taskInfo_hello_Hello_0 = {
    .eiid = "Hello",
    .max_endpoints = 0,
    .endpoints = NK_NULL,
};


static const EntityInfo taskInfo_Client = {
    .eiid = "Client",
    .max_endpoints = 0,
    .endpoints = NK_NULL,
};

static const EndpointInfo endpointsInfo_Server_1[2] = {
    {
        .name = "main",
        .riid = 0,
        .iface_name = "Echo",
    },
    {
        .name = "Server.main",
        .riid = 0,
        .iface_name = "Echo",
    }
};


static const EntityInfo taskInfo_Server = {
    .eiid = "Server",
    .max_endpoints = 2,
    .endpoints = endpointsInfo_Server_1,
};

int main(void) {

    const char * taskInfo_Server_args[] = {"Server", RTL_NULL};
    Entity *task_Server = EntityInitEx(&taskInfo_Server, "Server", "Server");

    if (!task_Server) {
        fprintf(stderr, "Can't initialize task \"Server\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task_Server, taskInfo_Server_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Server\"\n");
        return EXIT_FAILURE;
    }

   /*--------------------------------------------------------*/
    const char * taskInfo_Client_args[] = {"Client", RTL_NULL};
    Entity *task_Client = EntityInitEx(&taskInfo_Client, "Client", "Client");

    if (!task_Client) {
        fprintf(stderr, "Can't initialize task \"Client\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task_Client, taskInfo_Client_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Client\"\n");
        return EXIT_FAILURE;
    }

    Entity * task_hello_Hello_0;

    const char * taskInfo_hello_Hello_0_args[] = {
        "Hello",
        RTL_NULL
    };

    task_hello_Hello_0 = EntityInitEx(
        &taskInfo_hello_Hello_0, "Hello", "Hello"
    );
    if (!task_hello_Hello_0) {
        fprintf(stderr, "Can't initialize task \"Hello\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task_hello_Hello_0, taskInfo_hello_Hello_0_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Hello\"\n");
        return EXIT_FAILURE;
    }



    /*------------------------*/
    if (EntityConnectToService(task_Client, task_Server, "server_connection") != rcOk) {
        fprintf(stderr, "Can't setup a connection %s\n", "server_connection");
        return EXIT_FAILURE;
    }

    if (EntityConnectToService(task_hello_Hello_0, task_Server, "server_connection") != rcOk) {
        fprintf(stderr, "Can't setup a connection %s\n", "server_connection");
        return EXIT_FAILURE;
    }


    /*------------------------*/
    if (EntityRun(task_Server) != rcOk) {
        fprintf(stderr, "Can't run task \"Server\"\n");
        return EXIT_FAILURE;
    }

    if (EntityRun(task_Client) != rcOk) {
        fprintf(stderr, "Can't run task \"Client\"\n");
        return EXIT_FAILURE;
    }
    if (EntityRun(task_hello_Hello_0) != rcOk) {
        fprintf(stderr, "Can't run task \"Hello\"\n");
        return EXIT_FAILURE;
    }


    return EXIT_SUCCESS;
}
