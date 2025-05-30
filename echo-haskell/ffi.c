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





static struct ffi_entry table[] = {
{ "syscallCall",            syscallCall},
{ "serverLocatorConnect",  serverLocatorConnect},
{ "serviceLocatorGetRiid", serviceLocatorGetRiid},
{ "nkArenaInit",           nkArenaInit},
{ "nkArenaStoreString",    nkArenaStoreString},
{ "nkFillEnvelope",        nkFillEnvelope},
{ 0,0 }
};
struct ffi_entry *xffi_table = table;

