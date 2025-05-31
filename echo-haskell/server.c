#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Server.edl.h"

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

    do
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

            struct nk_message *request = &req.base_;
            struct nk_message *response = &res.base_;
            nk_err_t rc = NK_ENOENT;
            nk_iid_t riid = request->iid;
            nk_mid_t mid = request->mid;


            switch (riid) {
                case Server_main_iid:
                    switch (mid) {
                        case Echo_Echo_mid:
                            {

                                struct Echo_Echo_res *res_ = (struct Echo_Echo_res *) response;
                                struct Echo_Echo_req *req_ = (struct Echo_Echo_req *) request;

                                nk_uint32_t msgLen = 0;
                                nk_char_t *msg = nk_arena_get(nk_char_t, &req_arena, &req_->value, &msgLen);
                                if (msg == RTL_NULL) {
                                    fprintf(stderr, "[Server]: Error: can`t get message from arena!\n");
                                    rc = NK_EBADMSG;
                                } else {
                                    fprintf(stderr, "%s\n", msg);
                                    rc = NK_EOK;

                                }

                                if (rc == NK_EOK) {
                                    if (nk_msg_check_err(response)) {
                                        nk_err_reset(&res_->err_);
                                        nk_msg_set_ncaps(res_, Echo_Echo_err_handles);
                                    } else {
                                        nk_req_reset(&res_->res_);
                                        nk_msg_set_ncaps(res_, Echo_Echo_res_handles);
                                    }
                                }
                            }

                            break;
                        default:
                            fprintf(stderr, "unknown riid %d\n", request->iid);
                            break;
                    }
                    break;
                default:
                    fprintf(stderr, "unknown riid %d\n", request->iid);
                }
            }


            /* Send response. */
            if (nk_transport_reply(&transport.base,
                                &res.base_,
                                &res_arena) != NK_EOK) {
                fprintf(stderr, "nk_transport_reply error\n");
            }
        }
        while (true);

        return EXIT_SUCCESS;
    }
