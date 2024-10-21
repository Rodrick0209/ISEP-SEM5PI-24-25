using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.OperationRequest
{


    public class Priority : IValueObject
    {


        public string priority { get; private set; }

        public Priority(string priority)
        {
            validatePriority(priority);
            this.priority = priority;
        }

        private void validatePriority(string priority){
            priority.ToLower();
            List<string> priorities = new List<string> {"eletric", "urgency", "emergency"};
            if(string.IsNullOrEmpty(priority) || !priorities.Contains(priority)){
                throw new ArgumentNullException("Invalid priority");
            }
        }


    }









}