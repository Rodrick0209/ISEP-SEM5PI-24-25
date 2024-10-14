using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Patient;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class PatientsController : ControllerBase
    {
        private readonly IPatientService _service;

        public PatientsController(PatientService service)
        {
            _service = service;
        }

        // POST: api/Patients
        [HttpPost]
        public async Task<ActionResult<PatientDto>> Create(CreatingPatientProfileDto dto)
        {
            try
            {
                var patient = await _service.CreateAsync(dto);

                return Ok(patient);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Patients
        [HttpPut("{medicalRecordNumber}")]
        public async Task<ActionResult<PatientDto>> Update(string medicalRecordNumber, EditingPatientProfileDto dto)
        {
            if (medicalRecordNumber != dto.MedicalRecordNumber)
            {
                return BadRequest();
            }

            try
            {
                var patient = await _service.UpdateAsync(dto);

                return Ok(patient);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

    }
}