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

int main(void) {
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

    if (EntityRun(task_hello_Hello_0) != rcOk) {
        fprintf(stderr, "Can't run task \"Hello\"\n");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
