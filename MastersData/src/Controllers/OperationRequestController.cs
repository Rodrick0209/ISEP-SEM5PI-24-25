using System.Threading;
using System.Threading.Tasks;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Identity.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using System;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Authorization;
using System.Security.Claims;



namespace DDDSample1.Controllers
{
    
    [Route("api/[controller]")]
    [ApiController]

    public class OperationRequestController : ControllerBase
    {

        private readonly OperationRequestService _service;

        public OperationRequestController(OperationRequestService service)
        {
            _service = service;
        }

        [HttpPost]
        public async Task<ActionResult<OperationRequestDto>> Create(OperationRequestDto dto)
        {
            
            var objDomain = OperationRequestMapper.toDomain(dto);
            var op = await _service.AddAsync(objDomain);
            var op2 = OperationRequestMapper.toDTO(op);
            return CreatedAtAction(nameof(GetGetById), new { id = op2.Id }, op2);
        }


        [HttpGet("{id}")]

        public async Task<ActionResult<OperationRequestDto>> GetGetById(String id)
        {
            var op = await _service.GetByIdAsync(new OperationRequestId(id));
            if (op == null)
            {
                return NotFound();
            }
            return OperationRequestMapper.toDTO(op);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<OperationRequestDto>> Update(Guid id, OperationRequestDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var op = await _service.UpdateAsync(dto);
                if (op == null)
                {
                    return NotFound();
                }
                return Ok(OperationRequestMapper.toDTO(op));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        [HttpDelete("{id}")]

        public async Task<ActionResult<OperationRequestDto>> Delete(Guid id)
        {
            try
            {
                var op = await _service.DeleteAsync(new OperationRequestId(id));
                if (op == null)
                {
                    return NotFound();
                }

                return Ok(OperationRequestMapper.toDTO(op));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }



        }


        [HttpGet]
        [Authorize(Roles = "Doctor")]
        public async Task<IActionResult> GetOperationRequests([FromQuery] OperationRequestFilterDto filters)
        {
        var doctorId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;

        if (doctorId == null)
        {
            return Unauthorized();
        }

        var operationRequests = await _service.GetOperationRequestsWithFilters(filters, doctorId);
        return Ok(operationRequests);
    }



    }







}