using System.Threading;
using System.Threading.Tasks;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Identity.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using System;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Authorization;
using System.Security.Claims;
using DDDSample1.Domain.StaffMembers;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class StaffController : ControllerBase
    {
        private readonly IStaffService _service;

        public StaffController(IStaffService service)
        {
            _service = service;
        }

        [HttpPost]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<StaffDto>> Create(CreatingStaffDto dto)
        {

            try
            {
                var op = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = op.Id }, op);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }

        }

        
        [HttpPost("CreateUi")]
        [Authorize (Roles = "admin")]

        public async Task<ActionResult<StaffDto>> CreateUi(CreatingStaffDto dto)
        {
             try
            {
                var op = await _service.AddAsyncUi(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = op.Id }, op);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }

        }


        [HttpGet("{id}")]
        [Authorize(Roles = "admin")]

        public async Task<ActionResult<StaffDto>> GetGetById(String id)
        {
            var staff = await _service.GetByIdAsync(new StaffId(id));

            if (staff == null)
            {
                return NotFound();
            }

            return staff;
        }


        [HttpPut("{id}")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<StaffDto>> Update(EditingStaffProfileDto dto, String id)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var staff = await _service.UpdateAsync(dto);

                return Ok(staff);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpDelete("{id}")]
        [Authorize(Roles = "admin")]

        public async Task<ActionResult> Delete(string id)
        {
            try
            {
                var staff = await _service.DeleteAsync(new StaffId(id));
               return NoContent();
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }


        }


        // GET: api/Patients/search
        [HttpGet("search")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<IEnumerable<ViewStaffDto>>> SearchAsync([FromQuery] StaffFilterDto dto)
        {
            return await _service.SearchAsync(dto);
        }



        [HttpGet("GetAll")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<IEnumerable<StaffDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }


        [HttpGet("GetAllForUi")]
        [Authorize(Roles = "admin")]

        public async Task<ActionResult<IEnumerable<StaffDtoUI>>> GetAllForUi()
        {
        
            return await _service.GetAllForUiAsync();
        }


        [HttpGet("GetByIdForUI/{id}")]
        [Authorize(Roles = "admin")]

        public async Task<ActionResult<StaffDtoUI>> GetByIdForUI(string id)
        {
            var staff = await _service.GetByIdForUIAsync(new StaffId(id));

            if (staff == null)
            {
                return NotFound();
            }

            return staff;
        }
    }
}