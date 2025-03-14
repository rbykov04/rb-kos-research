#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Echo.idl.h"

#include <assert.h>


typedef struct NkKosTransportSDK {
    struct nk_transport  base;            /**< NK transport                    */
    Handle               handle;          /**< connection handle: client       *
                                           *   or server or listener           */
    Handle               reply_handle;    /**< server handle for reply         */
    Tid                  thread;          /**< client thread ID (tid)          */
    nk_uint32_t          recvTimeout;     /**< timeout for Recv                */
    nk_uint32_t          callTimeout;     /**< timeout for Call                */
    Handle               callSyncHandle;  /**< synchronization handle for Call */
    Handle               recvSyncHandle;  /**< synchronization handle for Recv */
} NkKosTransportSDK;

int main(void)
{
    Handle handle = ServiceLocatorConnect("server_connection");
    assert(handle != INVALID_HANDLE);

    NkKosTransport transport = {
        .base = {},
        .handle = handle
    };
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);

    nk_iid_t riid = ServiceLocatorGetRiid(handle, "Server.main");
    assert(riid != INVALID_RIID);

    struct Echo_proxy proxy = {
        .base = {},
        .transport = &transport.base,
        .iid = riid
    };

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


    nk_req_reset(&req);
    nk_msg_set_method_id(&req, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&req, Echo_Echo_req_handles);
    nk_req_reset(&res);
    nk_msg_set_method_id(&res, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&res, Echo_Echo_res_handles);

    nk_err_t rc = transport.base.ops->call (&transport.base
                                   , &req.base_
                                   , &reqArena
                                   , &res.base_
                                   , NULL);
    if (rc != rcOk) {
        fprintf(stderr, "Failed to call Server.Echo() with %d\n", rc);
    }

    return EXIT_SUCCESS;
}
