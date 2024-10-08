

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Users;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.Extensions.Configuration;
using DDDSample1.Domain.User;

namespace DDDSample1.Domain.User
{
 
public  class UserService
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly IUserRepository _repo;

    private readonly IConfiguration _configuration;

    public UserService(IUnitOfWork unitOfWork, IUserRepository repo,IConfiguration configuration)
    {
        _unitOfWork = unitOfWork;
        _repo = repo;
        _configuration = configuration;
    }

    public UserService(IUnitOfWork unitOfWork, IUserRepository repo)
    {
        _unitOfWork = unitOfWork;
        _repo = repo;
    }
    

    public async Task<string> GetLogToken(LoginRequest request)
    {
        if (request == null)
        {
        throw new Exception("Login request is null");
        }
        User? user = await _repo.GetByEmailAsync(request.Email);
        if (user == null)
        {
            throw new Exception("User not found");
        }
        
        PasswordHasher passwordHasher = new();
        var result  = passwordHasher.VerifyPassword(user.password.password, request.Password);


        if (!result)
        {
            throw new Exception("Invalid password");
        }


        TokenProvider tokenProvider = new TokenProvider(_configuration);
        string token = tokenProvider.Create(user);




        return token; // Retorna apenas o token
    }




    //Metodo do serviço para registar o user no sistema
    public async Task<User> AddAsync(User user)
    {
                
            bool exists = await this._repo.CheckEmail(user.email.email);

            if(exists) throw new BusinessRuleValidationException("email or phone number already exists");        
            
            await _repo.AddAsync(user);

            await _unitOfWork.CommitAsync();

            return user;
    }

    
    public async Task<List<User>> GetAllAsync()
    {                   
            return await _repo.GetAllAsync();
    }


}


}