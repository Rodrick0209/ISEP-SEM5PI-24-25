using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Newtonsoft.Json;

namespace DDDSample1.Application.Dtos
{
    public class SpecializationDto
    {
        public string Id { get; set; }
        public string Name { get; set; }

        public SpecializationDto(string id, string name)
        {
            Id = id;
            Name = name;
        }

        [JsonConstructor]
        public SpecializationDto(string name)
        {
            Name = name;
        }
    }
}