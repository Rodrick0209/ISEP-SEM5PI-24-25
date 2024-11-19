import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { PlanningService, SurgeryRoom } from '../../services/planning.service';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './planning.component.html',
  styleUrls: ['./planning.component.css']
})
export class PlanningComponent implements OnInit {
  date: string = '';
  selectedSala: string = '';
  greetingMessage: string = '';
  salasDeCirurgia: any[] = []; // Atualizado para receber dados da API

  // Variável para a data mínima (hoje)
  today: string = '';

  constructor(
    private planningService: PlanningService,
    private router: Router,
    private messageService: MessageService
  ) {}

  ngOnInit() {
    // Obtém a data atual no formato YYYY-MM-DD
    const currentDate = new Date();
    const year = currentDate.getFullYear();
    const month = (currentDate.getMonth() + 1).toString().padStart(2, '0');
    const day = currentDate.getDate().toString().padStart(2, '0');
    this.today = `${year}-${month}-${day}`;

    // Busca as salas de cirurgia da API
    this.getSurgeryRooms();
  }

  agendarCirurgia() {
    if (this.date && this.selectedSala) {
      this.greetingMessage = `Surgery scheduled for ${this.date} in room ${this.selectedSala}`;
    } else {
      this.greetingMessage = 'Please fill in all fields before scheduling.';
    }
  }

  getSurgeryRooms() {
    this.planningService.getSurgeryRooms().subscribe({
      next: (data: SurgeryRoom[]) => {
        console.log('Rooms received from the API:', data);
        this.salasDeCirurgia = data; // Atualiza as salas com os dados da API
      },
      error: (err: any) => {
        console.error('Failed to fetch surgery rooms', err);
      }
    });
  }
}
