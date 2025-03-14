#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Echo.idl.h"

#include <assert.h>


int main(void)
{
    Handle handle = ServiceLocatorConnect("server_connection");
    assert(handle != INVALID_HANDLE);

    NkKosTransport transport;
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);

    nk_iid_t riid = ServiceLocatorGetRiid(handle, "Server.main");
    assert(riid != INVALID_RIID);

    struct Echo_proxy proxy;
    Echo_proxy_init(&proxy, &transport.base, riid);

    /* Request and response structures. */
    Echo_Echo_req req;
    Echo_Echo_res res;


    /* Prepare response structures:fixed part and arena. */
    char reqBuffer[Echo_Echo_req_arena_size];
    struct nk_arena reqArena = NK_ARENA_INITIALIZER(
                                reqBuffer, reqBuffer + sizeof(reqBuffer));



    #define MESSAGE_SIZE 100
    nk_char_t message[MESSAGE_SIZE] = "Hello from client to server!";
    nk_arena_store(nk_char_t
                  , &reqArena
                  , &(req.value)
                  , message
                  , MESSAGE_SIZE);

    //         package    method
    //              |     |
    nk_err_t ret = Echo_Echo(&proxy.base, &req, &reqArena, &res, NULL);
    if (ret != rcOk) {
        fprintf(stderr, "Failed to call Server.Echo() with %d\n", ret);
    }

    return EXIT_SUCCESS;
}
