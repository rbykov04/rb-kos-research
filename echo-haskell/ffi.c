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
typedef struct __nk_packed EchoReq {
            __nk_alignas(8)
            struct nk_message base_;
            __nk_alignas(4) nk_ptr_t value;
        } EchoReq;
static_assert(sizeof(EchoReq) == 32, "bad_Echo_Echo_req_size");
static_assert(nk_offsetof(EchoReq, base_) == 0, "bad_Echo_Echo_req_base__offset");
static_assert(nk_offsetof(EchoReq, value) == 24, "bad_Echo_Echo_req_value_offset");

#pragma pack(push, 8) /* Echo_Echo_res */
typedef struct Echo_Echo_err {
            __nk_alignas(8)
            struct nk_message base_;
        } Echo_Echo_err;
static_assert(sizeof(Echo_Echo_err) == 24, "bad_Echo_Echo_err_size");
static_assert(nk_offsetof(Echo_Echo_err, base_) == 0, "bad_Echo_Echo_err_base__offset");
typedef struct Echo_Echo_res {
            union {
                struct {
                    __nk_alignas(8)
                    struct nk_message base_;
                };
                struct {
                    __nk_alignas(8)
                    struct nk_message base_;
                } res_;
                struct Echo_Echo_err err_;
            };
        } Echo_Echo_res;
static_assert(sizeof(Echo_Echo_res) == 24, "bad_Echo_Echo_res_size");
static_assert(nk_offsetof(Echo_Echo_res, base_) == 0, "bad_Echo_Echo_res_base__offset");
#pragma pack(pop) /* Echo_Echo_res */




#define MESSAGE_SIZE 100


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

    EchoReq req;
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

