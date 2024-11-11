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
using System.Collections.Generic;
using System.Linq;
using Microsoft.IdentityModel.JsonWebTokens;
using System.Xml;
using DDDSample1.Domain.Utils;




namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]

    public class OperationRequestController : ControllerBase
    {

        private readonly IOperationRequestService _service;

        public OperationRequestController(IOperationRequestService service)
        {
            _service = service;
        }

        [HttpPost]
        [Authorize(Roles = "doctor")]
        public async Task<ActionResult<OperationRequestDto>> Create(OperationRequestDto dto)
        {
            try
            {
                var emailDoctorQuerCriar = User.Claims.FirstOrDefault(c => c.Type == ClaimTypes.Email)?.Value;
                if (emailDoctorQuerCriar == null)
                {
                    throw new Exception("Email do doutor não encontrado.");
                }

                var objDomain = OperationRequestMapper.toDomain(dto, new Email(emailDoctorQuerCriar).getFirstPartOfEmail().ToString());
                var op = await _service.AddAsync(objDomain);
                var op2 = OperationRequestMapper.toDTO(op);
                return CreatedAtAction(nameof(GetGetById), new { id = op2.Id }, op2);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
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
        [Authorize(Roles = "doctor")]
        public async Task<ActionResult<OperationRequestDto>> Update(Guid id, ChangeOperationRequestDto dto)
        {
            Console.WriteLine("ID cabeçalho: " + id);
            Console.WriteLine("ID corpo: " + dto.Id);
            if (id != dto.Id)
            {
                return BadRequest("ID mismatch");
            }

            try
            {
                var emailDoctorQuerEditar = User.Claims.FirstOrDefault(c => c.Type == ClaimTypes.Email)?.Value;

                var op = await _service.UpdateAsync(dto, emailDoctorQuerEditar);
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
        [Authorize(Roles = "doctor")]
        public async Task<ActionResult<OperationRequestDto>> Delete(Guid id)
        {
            try
            {
                var op = await _service.DeleteAsync(new OperationRequestId(id));

                return Ok(OperationRequestMapper.toDTO(op));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }



        }


        [HttpGet("getWithFilters")]
        [Authorize(Roles = "doctor")]
        public async Task<IActionResult> GetOperationRequests([FromQuery] OperationRequestFilterDto filters)
        {
            var emailDoctorQuerEditar = User.Claims.FirstOrDefault(c => c.Type == ClaimTypes.Email)?.Value;
            var operationRequests = await _service.GetOperationRequestsWithFilters(filters, emailDoctorQuerEditar);
            Console.WriteLine("Size opRequests: " + operationRequests.Count);
            return Ok(operationRequests);
        }


        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            var listDto = new List<OperationRequestDto>();

            foreach (var op in list)
            {
                listDto.Add(OperationRequestMapper.toDTO(op));
            }

            return Ok(listDto);
        }


        [HttpGet("GetAllForUi")]
        [Authorize(Roles = "doctor")]

        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> GetAllForUi()
        {
            var emailDoctorQuerEditar = User.Claims.FirstOrDefault(c => c.Type == ClaimTypes.Email)?.Value;
            return await _service.GetAllForUiAsync(emailDoctorQuerEditar);
        }





    }







}