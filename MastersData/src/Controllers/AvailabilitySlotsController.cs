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
using DDDSample1.Domain.AvailabilitySlots;




namespace DDDSample1.Controllers
{
    
    [Route("api/[controller]")]
    [ApiController]

    public class AvailabilitySlotsController : ControllerBase
    {

        private readonly AvailabilitySlotService _service;

        public AvailabilitySlotsController(AvailabilitySlotService service)
        {
            _service = service;
        }



    [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<AvailabilitySlotDto>>> GetAll()
        {
            var list = await  _service.GetAllAsync();
            var listDto = new List<AvailabilitySlotDto>();
            Console.WriteLine("Agendas disponiveis "+ list.Count);
            foreach (var op in list)
            {
                listDto.Add(AvailabilitySlotMapper.ToDTO(op));
            }

            return Ok(listDto);
        } 

    }
}
