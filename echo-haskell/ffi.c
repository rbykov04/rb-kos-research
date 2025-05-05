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

#include "build/Echo.idl.h"

#include <assert.h>

#define MESSAGE_SIZE 100
//


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

    Echo_Echo_req req;
    Echo_Echo_res res;
    nk_arena_store(nk_char_t
                  , reqArena
                  , &(req.value)
                  , message
                  , size );


    rtl_printf("Echo mid = %d \n", Echo_Echo_mid);
    nk_req_reset(&req);
    nk_msg_set_method_id(&req, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&req, Echo_Echo_req_handles);
    nk_req_reset(&res);
    nk_msg_set_method_id(&res, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&res, Echo_Echo_res_handles);

    SMsgHdr msgReq;
    SMsgHdr msgRes;

    PackOutMsg (&msgReq, &req.base_, reqArena);
    PackInMsg  (&msgRes, &res.base_, NULL);

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

