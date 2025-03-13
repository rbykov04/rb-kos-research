#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Server.edl.h"

static nk_err_t echo_impl(struct Echo                 *self,
                          const struct Echo_Echo_req  *req,
                          const struct nk_arena       *req_arena,
                          struct Echo_Echo_res        *res,
                          struct nk_arena             *res_arena)
{
    return NK_EOK;
}

int main(void)
{
    ServiceId iid;
    Handle handle = ServiceLocatorRegister("server_connection", NULL, 0, &iid);

    NkKosTransport transport;
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);


    Server_entity_req req;
    char req_buffer[Server_entity_req_arena_size];
    struct nk_arena req_arena = NK_ARENA_INITIALIZER(req_buffer,
                                        req_buffer + sizeof(req_buffer));

    Server_entity_res res;
    char res_buffer[Server_entity_res_arena_size];
    struct nk_arena res_arena = NK_ARENA_INITIALIZER(res_buffer,
                                        res_buffer + sizeof(res_buffer));


    static const struct Echo_ops ops = {.Echo = echo_impl};
    struct Echo impl = {&ops};

    Server_entity entity;
    Server_entity_init(&entity, &impl);

    fprintf(stderr, "Hello I'm server\n");

    //do
    {
        /* Reset the buffer containing the request and response. */
        nk_req_reset(&req);
        nk_arena_reset(&req_arena);
        nk_arena_reset(&res_arena);

        /* Wait for a request for the server program. */
        if (nk_transport_recv(&transport.base, &req.base_, &req_arena) != NK_EOK) {
            fprintf(stderr, "nk_transport_recv error\n");
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
