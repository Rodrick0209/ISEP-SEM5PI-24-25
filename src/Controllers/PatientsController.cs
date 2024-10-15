using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patient;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class PatientsController : ControllerBase
    {
        private readonly PatientService _service;

        public PatientsController(PatientService service)
        {
            _service = service;
        }

        // POST: api/Patients
        [HttpPost("Create")]
        public async Task<ActionResult<PatientDto>> Create(CreatingPatientProfileDto dto)
        {
            try
            {
                var patient = await _service.CreateAsync(dto);

                return CreatedAtAction(nameof(GetByMedicalRecordNumber), new { medicalRecordNumber = patient.MedicalRecordNumber }, patient);
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

        [HttpGet]
        public async Task<ActionResult<List<PatientDto>>> GetAllAsync()
        {
            return await _service.GetAllAsync();
        }

        [HttpGet("MedicalRecordNumber/{medicalRecordNumber}")]
        public async Task<ActionResult<PatientDto>> GetByMedicalRecordNumber(string medicalRecordNumber)
        {
            var patient = await _service.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            if (patient == null)
            {
                return NotFound();
            }

            return patient;
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<PatientDto>> GetById(string id)
        {
            var patient = await _service.GetByIdAsync(id);

            if (patient == null)
            {
                return NotFound();
            }

            return patient;
        }

    }
}