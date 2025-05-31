#include "MicroHs/src/runtime/mhsffi.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include <coresrv/syscalls.h>
#include <rtl/stdio.h>
#include <rtl/retcode_hr.h>
#include <services/rtl/nk_msg.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Server.edl.h"



void fillEnvelope(nk_message* envelope
                  , size_t size
                  , nk_iid_t riid
                  , nk_mid_t mid
                  , uint32_t ncaps)
{
    memset (envelope, 0 , size);
    nk_message_set_size(envelope, size);
    nk_message_make_req(envelope);
    nk_message_set_iid(envelope, riid);
    nk_message_set_mid(envelope, mid);
    nk_message_set_ncaps(envelope, ncaps);
}

void nkFillEnvelope(int s)
{
    fillEnvelope( mhs_to_Ptr(s, 0)   // envelope
                , mhs_to_Int(s, 1)     // size
                , mhs_to_CUShort(s, 2) // riid
                , mhs_to_CUShort(s, 3) // mid
                , mhs_to_Word(s, 4)    // ncaps
                );

}



int kernelCall( Handle handle
               , char *request
               , struct nk_arena *reqArena
               , char *response
               , struct nk_arena *resArena
               )
{

    SMsgHdr msgReq;
    SMsgHdr msgRes;

    PackOutMsg (&msgReq, (nk_message*) request, reqArena);
    PackInMsg  (&msgRes, (nk_message*) response, resArena);

    Retcode rc = Call (handle, &msgReq , &msgRes);

    //TODO: Move this to haskell
    if (rc != rcOk) {
        rtl_printf("Operation has failed with rc = "RETCODE_HR_FMT"\n", RETCODE_HR_PARAMS(rc));
    }

    return rc;
}

void syscallCall(int s)
{

    mhs_from_Int(s, 5, kernelCall( mhs_to_Int(s, 0)     // handle
                                  , mhs_to_Ptr(s, 1)     // request
                                  , mhs_to_Ptr(s, 2)     // ArenaStruct (req)
                                  , mhs_to_Ptr(s, 3)     // response
                                  , mhs_to_Ptr(s, 4)     // ArenaStruct(res)
                                  ));
}


void nkArenaStoreString(int s)
{
    nk_arena_store(nk_char_t
                  , mhs_to_Ptr(s, 0)     // arena
                  , mhs_to_Ptr(s, 1)     // begin of Req struct
                    + mhs_to_Int(s, 2)     // offset //TODO FIXME (move this to haskell)
                  , mhs_to_Ptr(s, 3)     // text
                  , mhs_to_Int(s, 4)     // size
                  );
}



void nkArenaInit(int s){
    nk_arena_init ( mhs_to_Ptr(s, 0)
                  , mhs_to_Ptr(s, 1)
                  , mhs_to_Ptr(s, 1) + mhs_to_Int(s, 2));

}

void serverLocatorConnect(int s)
{
    mhs_from_Int(s, 1, ServiceLocatorConnect(mhs_to_Ptr(s, 0)));    // connection
}

void serviceLocatorGetRiid(int s)
{

/*
 ServiceLocatorGetRiid will return type = nk_iid_t
 typedef nk_uint16_t nk_iid_t;
*/
    mhs_from_CUShort(s,
                    2,
                    ServiceLocatorGetRiid(mhs_to_Int(s, 0),   // handle
                                          mhs_to_Ptr(s, 1))); // endpoint

}

void serverLocatorRegister(int s)
{
    const char * connection = mhs_to_Ptr(s, 0);
    ServiceId iid;

    Handle handle = ServiceLocatorRegister(connection, NULL, 0, &iid);

    mhs_from_Int(s, 1, handle);
}



void serverMain(int s)
{

    Handle handle = mhs_to_Int(s, 0);

    Server_entity_req *req = mhs_to_Ptr(s, 1);
    struct nk_arena   *req_arena = mhs_to_Ptr(s, 2);

    Server_entity_res *res = mhs_to_Ptr(s, 3);
    struct nk_arena   *res_arena = mhs_to_Ptr(s, 4);


    NkKosTransport transport;
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);

    do
    {
        /* Reset the buffer containing the request and response. */
        nk_req_reset(req);
        nk_arena_reset(req_arena);
        nk_arena_reset(res_arena);

        /* Wait for a request for the server program. */


        nk_err_t ret = nk_transport_recv(&transport.base, &req->base_, req_arena);
        if (ret  != NK_EOK ) {
            fprintf(stderr, "nk_transport_recv error: %d\n", ret);
        } else {

            struct nk_message *request = &req->base_;
            struct nk_message *response = &res->base_;
            nk_err_t rc = NK_ENOENT;
            nk_iid_t riid = request->iid;
            nk_mid_t mid = request->mid;


            switch (riid) {
                case Server_main_iid:
                    switch (mid) {
                        case Echo_Echo_mid:
                            {

                                struct Echo_Echo_req *req_ = (struct Echo_Echo_req *) request;
                                struct Echo_Echo_res *res_ = (struct Echo_Echo_res *) response;

                                nk_uint32_t msgLen = 0;
                                nk_char_t *msg = nk_arena_get(nk_char_t, req_arena, &req_->value, &msgLen);
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
                                response->iid = riid;
                                response->mid = mid;
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
            if (nk_transport_reply(&transport.base, &res->base_, res_arena) != NK_EOK) {
                fprintf(stderr, "nk_transport_reply error\n");
            }
        }
        while (true);

}




static struct ffi_entry table[] = {
{ "syscallCall",           syscallCall},
{ "serverLocatorConnect",  serverLocatorConnect},
{ "serverLocatorRegister",  serverLocatorRegister},
{ "serviceLocatorGetRiid", serviceLocatorGetRiid},
{ "nkArenaInit",           nkArenaInit},
{ "nkArenaStoreString",    nkArenaStoreString},
{ "nkFillEnvelope",        nkFillEnvelope},
{ "serverMain",            serverMain},
{ 0,0 }
};
struct ffi_entry *xffi_table = table;

