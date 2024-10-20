// using System.Threading;
// using System.Threading.Tasks;
// using DDDSample1.Domain.User;
// using Microsoft.AspNetCore.Identity.Data;
// using Microsoft.AspNetCore.Mvc;
// using Microsoft.Extensions.Configuration;
// using System;
// using DDDSample1.Domain.Shared;
// using Microsoft.AspNetCore.Authorization;
// using System.Security.Claims;
// using DDDSample1.Domain.StaffMembers;



// namespace DDDSample1.Controllers
// {
    
//     [Route("api/[controller]")]
//     [ApiController]

//     public class StaffController : ControllerBase
//     {

//         private readonly StaffService _service;

//         public StaffController(StaffService service)
//         {
//             _service = service;
//         }

//         [HttpPost]
//         public async Task<ActionResult<StaffDto>> Create(StaffDto dto)
//         {
            
//             var objDomain = StaffMapper.toDomain(dto);
//             var op = await _service.AddAsync(objDomain);
//             var op2 = StaffMapper.toDTO(op);
//             return CreatedAtAction(nameof(GetGetById), new { id = op2.Id }, op2);
//         }

//     }
// }
