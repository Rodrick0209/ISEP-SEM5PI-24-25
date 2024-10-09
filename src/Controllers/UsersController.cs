using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Families;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]


    public class UsersController : ControllerBase
    {

        private readonly UserService _service;
        
        public UsersController(UserService service)
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
                
                Console.WriteLine($"User created with ID: {user.Id}");
                return UserMapper.ToDto(user);

            }
            catch (BusinessRuleValidationException ex)
            {
                Console.WriteLine($"Validation error: {ex.Message}");
                return Conflict(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Unexpected error: {ex}");
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // GET: api/user
        [HttpGet("Get user")]
        [Authorize(Roles = "admin")]
        public async Task<List<UserDTO>> GetAllAsync()
        {

            var list = await _service.GetAllAsync();            
            List<UserDTO> listDto = list.ConvertAll<UserDTO>(User => 
                UserMapper.ToDto(User));

            return listDto;
        }



    }




}