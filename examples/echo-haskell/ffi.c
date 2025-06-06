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


void getEnvelopeMid(int s)
{
    nk_message *envelope = mhs_to_Ptr(s, 0);
    mhs_from_CUShort(s, 1, envelope->mid);
    mhs_from_CUShort(s, 1, -1);
}

void setEnvelopeMid(int s)
{
    nk_message *envelope = mhs_to_Ptr(s, 0);
    envelope->mid = mhs_to_CUShort(s, 1);
}

void getEnvelopeRiid(int s)
{
    nk_message *envelope = mhs_to_Ptr(s, 0);
    mhs_from_CUShort(s, 1, envelope->iid);
}

void setEnvelopeRiid(int s)
{
    nk_message *envelope = mhs_to_Ptr(s, 0);
    envelope->iid = mhs_to_CUShort(s, 1);
}






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

void nkArenaGetString(int s)
{

    nk_ptr_t          *value     = mhs_to_Ptr(s, 0) + mhs_to_Int(s, 2);
    struct nk_arena   *arena     = mhs_to_Ptr(s, 1);

    nk_uint32_t msgLen = 0;
    nk_char_t *msg = nk_arena_get(nk_char_t, arena, value, &msgLen);
    mhs_from_Ptr (s, 3, msg);
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

void syscallRecv(int s)
{
    Handle            handle    = mhs_to_Int(s, 0);
    nk_message       *req       = mhs_to_Ptr(s, 1);
    struct nk_arena  *req_arena = mhs_to_Ptr(s, 2);

    SMsgHdr msg;
    PackInMsg (&msg, req, req_arena);
    Retcode rc = Recv (handle, &msg);
    if (rc != rcOk) {
        rtl_printf("Recv operation has failed with rc = "RETCODE_HR_FMT"\n", RETCODE_HR_PARAMS(rc));
    }

    mhs_from_Int(s, 3, rc);
}

void syscallReply(int s)
{
    Handle            handle    = mhs_to_Int(s, 0);
    nk_message       *res       = mhs_to_Ptr(s, 1);
    struct nk_arena  *res_arena = mhs_to_Ptr(s, 2);

    SMsgHdr msg;
    PackOutMsg (&msg, res, res_arena);
    Retcode rc = Reply (handle, &msg);
    if (rc != rcOk) {
        rtl_printf("Reply operation has failed with rc = "RETCODE_HR_FMT"\n", RETCODE_HR_PARAMS(rc));
    }

    mhs_from_Int(s, 3, rc);
}





static struct ffi_entry table[] = {
{ "syscallCall",           syscallCall},
{ "syscallRecv",           syscallRecv},
{ "syscallReply",          syscallReply},
{ "serverLocatorConnect",  serverLocatorConnect},
{ "serverLocatorRegister", serverLocatorRegister},
{ "serviceLocatorGetRiid", serviceLocatorGetRiid},
{ "nkArenaInit",           nkArenaInit},
{ "nkArenaStoreString",    nkArenaStoreString},
{ "nkFillEnvelope",        nkFillEnvelope},
{ "nkEnvelopeMid",         getEnvelopeMid},
{ "nkSetEnvelopeMid",      setEnvelopeMid},
{ "nkEnvelopeRiid",        getEnvelopeRiid},
{ "nkSetEnvelopeRiid",     setEnvelopeRiid},
{ "nkArenaGetString",      nkArenaGetString},
{ 0,0 }
};
struct ffi_entry *xffi_table = table;

