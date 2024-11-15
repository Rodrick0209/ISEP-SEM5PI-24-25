#nullable enable
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;
using DDDSample1.Domain.Patients;
using System.Web;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]


    public class UsersController : ControllerBase
    {

        private readonly IUserService _service;

        public UsersController(IUserService service)
        {
            _service = service;
        }




        // POST: api/user
        [HttpPost("Create user")]
        public async Task<ActionResult<UserDTO>> Create(UserDTO dto)
        {
            try
            {

                if (!ModelState.IsValid) return BadRequest(ModelState);

                var user = await _service.AddAsync(UserMapper.ToDomain(dto));

                return UserMapper.ToDto(user);

            }
            catch (BusinessRuleValidationException ex)
            {
                return Conflict(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // GET: api/user
        [HttpGet("Get user")]
        public async Task<List<UserDTO>> GetAllAsync()
        {

            var list = await _service.GetAllAsync();
            List<UserDTO> listDto = list.ConvertAll<UserDTO>(User =>
                UserMapper.ToDto(User));

            return listDto;
        }


        [Route("Forgot-Password")]
        [HttpPost]
        public async Task<ActionResult> ForgotPassword(RequestForgotPasswordDto dto)
        {
            var user = await _service.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                return BadRequest(new { Message = "Invalid email" });
            }

            await _service.sendEmailWithUrlResetPassword(user.email.email);

            return Ok(new
            {
                email = user.email.email,
            });
        }

        [Route("Reset-Password")]
        [HttpPost]
        public async Task<ActionResult> ResetPassword(ResetPasswordRequestDto dto)
        {
            if (!ModelState.IsValid)
                return BadRequest("Invalid payload");

            var user = await _service.GetByEmailAsync(dto.Email);
            if (user == null)
            {
                return BadRequest("Invalid payload");
            }
            if (dto.NewPassword == null || dto.Token == null)
            {
                return BadRequest("Invalid payload");
            }

            try
            {
                var result = await _service.ResetPassword(user, dto.NewPassword, dto.Token);

                return Ok(new { Message = "Password reset successfully" });

            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }


        }

        // POST: api/user/patients
        [HttpPost("patients")]
        public async Task<ActionResult<ConfirmationPatientDto>> RegisterPatientAsync(RegisteringPatientDto dto)
        {
            try
            {
                var confirmationRegisterPatientDto = await _service.RegisterPatientAsync(dto);

                return Ok(confirmationRegisterPatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        [HttpPost("patients/confirm")]
        public async Task<ActionResult> ConfirmRegisterPatientAsync([FromQuery] ConfirmationPatientDto dto)
        {
            try
            {
                var userDto = await _service.ConfirmRegisterPatientAsync(dto);

                return Ok(userDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PATCH : api/user/patients/edit
        [HttpPatch("patients/edit")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<ConfirmationEditPatientDto>> EditPatientAsync(EditingPatientDto dto)
        {
            try
            {
                var confirmationEditPatientDto = await _service.EditPatientAsync(dto);

                return Ok(confirmationEditPatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        [HttpPatch("patients/edit/confirm")]
        public async Task<ActionResult<PatientDto>> ConfirmEditPatientAsync([FromQuery] ConfirmationEditPatientDto dto)
        {
            try
            {
                var patientDto = await _service.ConfirmEditPatientAsync(dto);

                return Ok(patientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // DELETE: api/user/patients/delete
        [HttpDelete("patients/delete/{email}")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<DeletingPatientProfileConfirmationDto>> DeletePatientAsync(string email)
        {
            try
            {
                var confirmationDeletePatientDto = await _service.DeletePatientAsync(email);

                return Ok(confirmationDeletePatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // DELETE: api/user/patients/delete/confirmation/{token}
        [HttpDelete("patients/delete/confirm")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult> ConfirmDeletePatientAsync([FromQuery] ConfirmationPatientDto dto)
        {
            try
            {
                await _service.ConfirmDeletePatientAsync(dto);

                return NoContent();
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // GET: api/user/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<UserDTO>> GetByIdAsync(Guid id)
        {
            var user = await _service.GetByIdAsync(new UserId(id));

            if (user == null)
            {
                return NotFound();
            }

            return Ok(user);
        }
    }
}
