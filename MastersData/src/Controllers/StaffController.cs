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

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class StaffController : ControllerBase
    {
        private readonly StaffService _service;

        public StaffController(StaffService service)
        {
            _service = service;
        }

        [HttpPost]
        public async Task<ActionResult<StaffDto>> Create(StaffDto dto)
        {
            var op = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = op.Id }, op);
        }


        [HttpGet("{id}")]

        public async Task<ActionResult<StaffDto>> GetGetById(String id)
        {
            var op = await _service.GetByIdAsync(new StaffId(id));
            if (op == null)
            {
                return NotFound();
            }
            return StaffMapper.toDTO(op);
        }

/*
        [HttpPut("{id}")]
        public async Task<ActionResult<StaffDto>> Update(StaffId id, EditingStaffProfileDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var staff = await _service.UpdateAsync(dto, dto.Id);

                return Ok(staff);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }*/

        [HttpDelete("{id}")]

        public async Task<ActionResult<StaffDto>> Delete(Guid id)
        {
            try
            {
                var staff = await _service.DeleteAsync(new StaffId(id));
                if (staff == null)
                {
                    return NotFound();
                }

                return Ok(StaffMapper.toDTO(staff));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }



        }

         [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<StaffDto>>> GetAll()
        {
            var list = await  _service.GetAllAsync();
            var listDto = new List<StaffDto>();
            
            foreach (var staff in list)
            {
                listDto.Add(StaffMapper.toDTO(staff));
            }

            return Ok(listDto);
        } 
    }
}