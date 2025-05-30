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

enum {
    Echo_Echo_mid,
    Echo_mid_max,
};
enum {
    Echo_Echo_req_value_size = 257,
    Echo_Echo_req_arena_size = 257,
    Echo_Echo_res_arena_size = 0,
    Echo_Echo_req_handles = 0,
    Echo_Echo_res_handles = 0,
    Echo_Echo_err_handles = 0,
    Echo_req_arena_size = 257,
    Echo_res_arena_size = 0,
    Echo_req_handles = 0,
    Echo_res_handles = 0,
    Echo_err_handles = 0,
};

#define MESSAGE_SIZE 100

void fillEnvelope(nk_message* envelope
                  , size_t size
                  , nk_iid_t riid
                  , nk_mid_t mid
                  , uint32_t ncaps)
{
    nk_message_set_size(envelope, size);
    nk_message_make_req(envelope);
    nk_message_set_iid(envelope, riid);
    nk_message_set_mid(envelope, mid);
    nk_message_set_ncaps(envelope, ncaps);
}

int sendToHello( Handle handle
               , nk_iid_t riid
               , const char *message
               , int size
               , struct nk_arena *reqArena
               )
{
    assert(handle != INVALID_HANDLE);
    assert(riid != INVALID_RIID);
    assert(reqArena != NULL);

    char request[32] = {0};
    fillEnvelope ((nk_message*) &request
                 , sizeof(request)
                 , riid
                 , Echo_Echo_mid
                 , Echo_Echo_req_handles);

    nk_ptr_t   *value    = (nk_ptr_t*) &request[24];
    nk_arena_store(nk_char_t
                  , reqArena
                  , value
                  , message
                  , size );

    char response[24] = {0};

    fillEnvelope ((nk_message*) &response
                 , sizeof(response)
                 , riid
                 , Echo_Echo_mid
                 , Echo_Echo_res_handles);

    SMsgHdr msgReq;
    SMsgHdr msgRes;

    PackOutMsg (&msgReq, (nk_message*) &request, reqArena);
    PackInMsg  (&msgRes, (nk_message*) &response, NULL);

    Retcode rc = Call (handle, &msgReq , &msgRes);
    if (rc != rcOk) {
        rtl_printf("Operation has failed with rc = "RETCODE_HR_FMT"\n", RETCODE_HR_PARAMS(rc));
    }

    return EXIT_SUCCESS;
}

void hello(int s)
{
    mhs_from_Int(s, 5, sendToHello( mhs_to_Int(s, 0)     // handle
                                  , mhs_to_CUShort(s, 1) // riid
                                  , mhs_to_Ptr(s, 2)     // text
                                  , mhs_to_Int(s, 3)     // size
                                  , mhs_to_Ptr(s, 4)     // ArenaStruct
                                  ));
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





static struct ffi_entry table[] = {
{ "hello", hello},
{ "serverLocatorConnect",  serverLocatorConnect},
{ "serviceLocatorGetRiid", serviceLocatorGetRiid},
{ "nkArenaInit",           nkArenaInit},
{ 0,0 }
};
struct ffi_entry *xffi_table = table;

