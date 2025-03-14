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

int sendToHello( const char *connection
               , const char *message
               , int size)
{
    Handle handle = ServiceLocatorConnect(connection);
    assert(handle != INVALID_HANDLE);

    nk_iid_t riid = ServiceLocatorGetRiid(handle, "Server.main");
    assert(riid != INVALID_RIID);

    /* Request and response structures. */
    Echo_Echo_req req;
    Echo_Echo_res res;

    /* Prepare response structures:fixed part and arena. */
    char reqBuffer[Echo_Echo_req_arena_size];
    struct nk_arena reqArena = NK_ARENA_INITIALIZER(
                                reqBuffer, reqBuffer + sizeof(reqBuffer));

    assert (size < MESSAGE_SIZE-1);
    char buf[MESSAGE_SIZE] = {};
    strncpy(buf, message, size);
    buf[size+1] = 0;
    nk_arena_store(nk_char_t
                  , &reqArena
                  , &(req.value)
                  , buf
                  , size + 1);


    nk_req_reset(&req);
    nk_msg_set_method_id(&req, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&req, Echo_Echo_req_handles);
    nk_req_reset(&res);
    nk_msg_set_method_id(&res, riid, Echo_Echo_mid);
    nk_msg_set_ncaps(&res, Echo_Echo_res_handles);

    SMsgHdr msgReq;
    SMsgHdr msgRes;

    PackOutMsg (&msgReq, &req.base_, &reqArena);
    PackInMsg  (&msgRes, &res.base_, NULL);

    Retcode rc = Call (handle, &msgReq , &msgRes);
    if (rc != rcOk) {
        rtl_printf("Operation has failed with rc = "RETCODE_HR_FMT"\n", RETCODE_HR_PARAMS(rc));
    }

    return EXIT_SUCCESS;
}

void hello(int s)
{
    mhs_from_Int(s, 3, sendToHello( mhs_to_Ptr(s, 0)    // connection
                                  , mhs_to_Ptr(s, 1)    // text
                                  , mhs_to_Int(s, 2))); // size
}

static struct ffi_entry table[] = {
{ "hello", hello},
{ 0,0 }
};
struct ffi_entry *xffi_table = table;
