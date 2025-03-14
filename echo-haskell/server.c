#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Server.edl.h"

static nk_err_t echo_impl(struct Echo                 *self,
                          const struct Echo_Echo_req  *req,
                          const struct nk_arena       *req_arena,
                          struct Echo_Echo_res        *res,
                          struct nk_arena             *res_arena)
{
    nk_uint32_t msgLen = 0;
    nk_char_t *msg = nk_arena_get(nk_char_t, req_arena, &req->value, &msgLen);
    if (msg == RTL_NULL) {
        fprintf(stderr, "[Server]: Error: can`t get message from arena!\n");
        return NK_EBADMSG;
    }
    fprintf(stderr, "%s\n", msg);
    return NK_EOK;
}

int main(void)
{
    ServiceId iid;
    Handle handle = ServiceLocatorRegister("server_connection", NULL, 0, &iid);
    assert(handle != INVALID_HANDLE);

    NkKosTransport transport;
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);


   Server_entity_req req = {0};
    char req_buffer[Server_entity_req_arena_size] = {};
    struct nk_arena req_arena = NK_ARENA_INITIALIZER(req_buffer,
                                        req_buffer + sizeof(req_buffer));

    Server_entity_res res = {0};
    char res_buffer[Server_entity_res_arena_size] = {};
    struct nk_arena res_arena = NK_ARENA_INITIALIZER(res_buffer,
                                        res_buffer + sizeof(res_buffer));


    static const struct Echo_ops ops = {.Echo = echo_impl};
    struct Echo impl = {&ops};

    Server_entity entity;
    Server_entity_init(&entity, &impl);


    //do
    {
        /* Reset the buffer containing the request and response. */
        nk_req_reset(&req);
        nk_arena_reset(&req_arena);
        nk_arena_reset(&res_arena);

        /* Wait for a request for the server program. */


        nk_err_t ret = nk_transport_recv(&transport.base, &req.base_, &req_arena);
        if (ret  != NK_EOK ) {
            fprintf(stderr, "nk_transport_recv error: %d\n", ret);
        } else {
            Server_entity_dispatch(&entity, &req.base_, &req_arena,
                                        &res.base_, &res_arena);
        }


        /* Send response. */
        if (nk_transport_reply(&transport.base,
                               &res.base_,
                               &res_arena) != NK_EOK) {
            fprintf(stderr, "nk_transport_reply error\n");
        }
    }
    //while (true);

    return EXIT_SUCCESS;
}
